// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::collections::VecDeque;

use futures::{
    future::{self, AbortHandle, Abortable},
    Future, Stream,
};
use serde_json::{Map as JsonMap, Value as JsonValue};

mod actor;
pub mod scheduler;

pub use actor::*;

pub trait Matcher<T>
where
    Self: Sized,
{
    fn match_type(&self) -> Option<&T>;
    fn into_type(self) -> Option<T>;
    fn try_into_type(self) -> Result<T, Self>;
}
impl<T, T2> Matcher<T2> for T
where
    Option<T2>: From<T>,
    for<'a> Option<&'a T2>: From<&'a T>,
{
    fn match_type(&self) -> Option<&T2> {
        self.into()
    }
    fn into_type(self) -> Option<T2> {
        self.into()
    }
    fn try_into_type(self) -> Result<T2, T> {
        if self.match_type().is_some() {
            Ok(self.into_type().unwrap())
        } else {
            Err(self)
        }
    }
}

pub trait Action {}

pub trait InboundAction<T>: Matcher<T> {}
impl<T, T2> InboundAction<T2> for T where T: Matcher<T2> {}

pub trait OutboundAction<T>: From<T> {}
impl<T, T2> OutboundAction<T2> for T where T: From<T2> {}

pub trait NamedAction: Action {
    fn name(&self) -> &'static str;
}

pub trait SerializableAction: NamedAction {
    fn serialize(&self) -> SerializedAction;
}

pub struct SerializedAction {
    inner: Vec<(String, JsonValue)>,
}
impl From<SerializedAction> for JsonValue {
    fn from(value: SerializedAction) -> Self {
        JsonValue::Object(JsonMap::from_iter(value.inner))
    }
}
impl FromIterator<(&'static str, JsonValue)> for SerializedAction {
    fn from_iter<T: IntoIterator<Item = (&'static str, JsonValue)>>(iter: T) -> Self {
        Self {
            inner: iter
                .into_iter()
                .map(|(key, value)| (String::from(key), value))
                .collect(),
        }
    }
}
impl IntoIterator for SerializedAction {
    type Item = (String, JsonValue);
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

pub trait Actor<T: Action> {
    fn handle(
        &mut self,
        action: &T,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> StateTransition<T>;
}

#[derive(Clone, Copy, Debug)]
pub struct MessageData {
    pub offset: MessageOffset,
    pub parent: Option<MessageOffset>,
    pub timestamp: std::time::Instant,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default, Debug)]
pub struct MessageOffset(usize);
impl MessageOffset {
    pub fn next(&self) -> MessageOffset {
        let Self(value) = self;
        Self(*value + 1)
    }
}
impl From<usize> for MessageOffset {
    fn from(value: usize) -> Self {
        Self(value)
    }
}
impl From<MessageOffset> for usize {
    fn from(value: MessageOffset) -> Self {
        let MessageOffset(value) = value;
        value
    }
}
impl From<MessageOffset> for JsonValue {
    fn from(value: MessageOffset) -> Self {
        JsonValue::from(Into::<usize>::into(value))
    }
}

pub struct StateTransition<T: Action> {
    operations: Vec<StateOperation<T>>,
}
impl<T: Action> Default for StateTransition<T> {
    fn default() -> Self {
        Self {
            operations: Default::default(),
        }
    }
}
impl<T: Action> StateTransition<T> {
    pub fn new(operations: impl IntoIterator<Item = StateOperation<T>>) -> Self {
        Self {
            operations: operations.into_iter().collect(),
        }
    }
    pub fn append(self, other: StateTransition<T>) -> StateTransition<T> {
        let mut combined = self;
        combined.operations.extend(other.operations);
        combined
    }
}
impl<T: Action> IntoIterator for StateTransition<T> {
    type Item = StateOperation<T>;
    type IntoIter = std::vec::IntoIter<StateOperation<T>>;
    fn into_iter(self) -> Self::IntoIter {
        self.operations.into_iter()
    }
}

pub enum StateOperation<T: Action> {
    Send(ProcessId, T),
    Task(ProcessId, OperationStream<T>),
    Spawn(ProcessId, WorkerFactory<T>),
    Kill(ProcessId),
}

pub type DisposeFuture = Box<dyn Future<Output = ()> + Send + Unpin + 'static>;

pub trait DisposeCallback: Send + 'static {
    fn dispose(self) -> DisposeFuture;
}
impl<T> DisposeCallback for T
where
    T: FnOnce() + Send + 'static,
{
    fn dispose(self) -> DisposeFuture {
        self();
        Box::new(future::ready(()))
    }
}
impl DisposeCallback for AbortHandle {
    fn dispose(self) -> DisposeFuture {
        self.abort();
        Box::new(future::ready(()))
    }
}
pub struct NoopDisposeCallback;
impl DisposeCallback for NoopDisposeCallback {
    fn dispose(self) -> DisposeFuture {
        Box::new(future::ready(()))
    }
}

pub struct BoxedDisposeCallback {
    inner: Box<dyn FnOnce() -> DisposeFuture + Send>,
}
impl BoxedDisposeCallback {
    fn new(inner: impl DisposeCallback) -> Self {
        Self {
            inner: Box::new(move || inner.dispose()),
        }
    }
}
impl DisposeCallback for BoxedDisposeCallback {
    fn dispose(self) -> DisposeFuture {
        (self.inner)()
    }
}

pub struct OperationStream<T: Action> {
    task: Box<dyn Stream<Item = StateOperation<T>> + Send + Unpin + 'static>,
    dispose: BoxedDisposeCallback,
}
impl<T: Action + 'static> OperationStream<T> {
    pub fn new(task: impl Stream<Item = StateOperation<T>> + Send + Unpin + 'static) -> Self {
        let (abort_handle, abort_registration) = AbortHandle::new_pair();
        let task = Abortable::new(task, abort_registration);
        Self {
            task: Box::new(task),
            dispose: BoxedDisposeCallback::new(abort_handle),
        }
    }
    pub fn disposable(
        task: impl Stream<Item = StateOperation<T>> + Send + Unpin + 'static,
        dispose: impl DisposeCallback,
    ) -> Self {
        Self {
            task: Box::new(task),
            dispose: BoxedDisposeCallback::new(dispose),
        }
    }
    pub(crate) fn from_parts(
        task: Box<dyn Stream<Item = StateOperation<T>> + Send + Unpin + 'static>,
        dispose: BoxedDisposeCallback,
    ) -> Self {
        Self { task, dispose }
    }
    pub(crate) fn into_parts(
        self,
    ) -> (
        Box<dyn Stream<Item = StateOperation<T>> + Send + Unpin + 'static>,
        BoxedDisposeCallback,
    ) {
        (self.task, self.dispose)
    }
}

pub trait Scheduler {
    type Action: Action;
    fn dispatch(&mut self, action: Self::Action);
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Default, Debug)]
pub struct ProcessId(usize);
impl ProcessId {
    pub fn next(&self) -> Self {
        let Self(value) = self;
        Self(value + 1)
    }
}
impl From<usize> for ProcessId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}
impl From<ProcessId> for usize {
    fn from(value: ProcessId) -> Self {
        let ProcessId(value) = value;
        value
    }
}
impl std::fmt::Display for ProcessId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(value) = self;
        write!(f, "<pid:{}>", value)
    }
}

pub trait HandlerContext {
    fn pid(&self) -> ProcessId;
    fn caller_pid(&self) -> Option<ProcessId>;
    fn generate_pid(&mut self) -> ProcessId;
}

pub trait Worker<T: Action> {
    fn handle(
        &mut self,
        queue: WorkerMessageQueue<T>,
        context: &mut WorkerContext,
    ) -> WorkerTransition<T>;
}

pub struct WorkerContext {
    inner: Box<dyn HandlerContext>,
}
impl WorkerContext {
    pub fn new(inner: impl HandlerContext + 'static) -> Self {
        Self {
            inner: Box::new(inner),
        }
    }
}
impl HandlerContext for WorkerContext {
    fn pid(&self) -> ProcessId {
        self.inner.pid()
    }
    fn caller_pid(&self) -> Option<ProcessId> {
        self.inner.caller_pid()
    }
    fn generate_pid(&mut self) -> ProcessId {
        self.inner.generate_pid()
    }
}

pub type WorkerMessageQueue<T> = VecDeque<(T, MessageData)>;

pub struct WorkerTransition<T: Action> {
    operations: Vec<(MessageOffset, StateOperation<T>)>,
}
impl<T: Action> WorkerTransition<T> {
    pub fn new(operations: impl IntoIterator<Item = (MessageOffset, StateOperation<T>)>) -> Self {
        Self {
            operations: operations.into_iter().collect(),
        }
    }
    pub fn append(self, other: WorkerTransition<T>) -> WorkerTransition<T> {
        let mut combined = self;
        combined.operations.extend(other.operations);
        combined
    }
}
impl<T: Action> IntoIterator for WorkerTransition<T> {
    type Item = (MessageOffset, StateOperation<T>);
    type IntoIter = std::vec::IntoIter<(MessageOffset, StateOperation<T>)>;
    fn into_iter(self) -> Self::IntoIter {
        self.operations.into_iter()
    }
}

pub struct WorkerFactory<TAction: Action> {
    factory: Box<dyn FnOnce() -> WorkerInstance<TAction> + Send + 'static>,
}
impl<TAction: Action + Send + 'static> WorkerFactory<TAction> {
    pub fn new<T: Worker<TAction> + Send + 'static>(
        factory: impl FnOnce() -> T + Send + 'static,
    ) -> Self {
        Self {
            factory: Box::new(|| Box::new(factory())),
        }
    }
    pub(crate) fn create(self) -> Box<dyn Worker<TAction> + Send + 'static> {
        (self.factory)()
    }
}

pub type WorkerInstance<TAction> = Box<dyn Worker<TAction> + Send + 'static>;

pub fn compose_actors<T: Action, T1: Actor<T>, T2: Actor<T>>(
    left: T1,
    right: T2,
) -> ChainedActor<T, T1, T2> {
    ChainedActor::new(left, right)
}

pub fn find_latest_typed_message<'a, T: Action, TAction: Action + InboundAction<T>>(
    queue: &mut WorkerMessageQueue<TAction>,
) -> (Option<(T, MessageData)>, WorkerMessageQueue<TAction>) {
    let mut trailing_actions = WorkerMessageQueue::default();
    while let Some((action, metadata)) = queue.pop_back() {
        match action.try_into_type() {
            Ok(action) => {
                return (Some((action, metadata)), trailing_actions);
            }
            Err(action) => {
                trailing_actions.push_front((action, metadata));
            }
        }
    }
    (None, trailing_actions)
}
