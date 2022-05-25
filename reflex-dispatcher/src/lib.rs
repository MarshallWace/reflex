// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::VecDeque, marker::PhantomData};

use futures::{
    future::{self, AbortHandle, Abortable},
    Future, Stream,
};
use serde_json::{Map as JsonMap, Value as JsonValue};

pub mod utils;

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
    type State;
    fn init(&self) -> Self::State;
    fn handle(
        &self,
        state: Self::State,
        action: &T,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, T>;
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

pub struct StateTransition<T: Action>(Vec<StateOperation<T>>);
impl<T: Action> Default for StateTransition<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<T: Action> StateTransition<T> {
    pub fn new(actions: impl IntoIterator<Item = StateOperation<T>>) -> Self {
        Self::from_iter(actions)
    }
    pub fn append(self, other: StateTransition<T>) -> StateTransition<T> {
        let Self(mut actions) = self;
        let Self(other_actions) = other;
        actions.extend(other_actions);
        Self(actions)
    }
}
impl<A: Action> FromIterator<StateOperation<A>> for StateTransition<A> {
    fn from_iter<T: IntoIterator<Item = StateOperation<A>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
impl<T: Action> IntoIterator for StateTransition<T> {
    type Item = StateOperation<T>;
    type IntoIter = std::vec::IntoIter<StateOperation<T>>;
    fn into_iter(self) -> Self::IntoIter {
        let Self(actions) = self;
        actions.into_iter()
    }
}

pub struct ActorTransition<S, T: Action> {
    state: S,
    actions: StateTransition<T>,
}
impl<S, T: Action> ActorTransition<S, T> {
    pub fn new(state: S, actions: StateTransition<T>) -> Self {
        Self { state, actions }
    }
    pub fn into_parts(self) -> (S, StateTransition<T>) {
        let Self { state, actions } = self;
        (state, actions)
    }
}

pub enum StateOperation<T: Action> {
    Send(ProcessId, T),
    Task(ProcessId, OperationStream<T>),
    Spawn(
        ProcessId,
        Box<dyn WorkerFactory<T, Worker = WorkerInstance<T>> + Send + 'static>,
    ),
    Kill(ProcessId),
}
impl<T: Action + Send + 'static> StateOperation<T> {
    pub fn spawn(
        pid: ProcessId,
        worker: impl WorkerFactory<T, Worker = impl Worker<T> + Send + 'static> + Send + 'static,
    ) -> Self {
        Self::Spawn(pid, Box::new(BoxedWorkerFactory::new(worker)))
    }
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

pub trait WorkerFactory<TAction: Action + Send + 'static> {
    type Worker: Worker<TAction>;
    fn create(&self) -> Self::Worker;
}
impl<TWorker, TAction> WorkerFactory<TAction>
    for Box<dyn WorkerFactory<TAction, Worker = TWorker> + Send + 'static>
where
    TWorker: Worker<TAction> + Send + 'static,
    TAction: Action + Send + 'static,
{
    type Worker = WorkerInstance<TAction>;
    fn create(&self) -> Self::Worker {
        let inner = &**self;
        Box::new(inner.create())
    }
}
pub struct BoxedWorkerFactory<TFactory, TWorker, TAction>
where
    TFactory: WorkerFactory<TAction, Worker = TWorker>,
    TWorker: Worker<TAction>,
    TAction: Action + Send + 'static,
{
    factory: TFactory,
    _worker: PhantomData<TWorker>,
    _action: PhantomData<TAction>,
}
impl<TFactory, TWorker, TAction> BoxedWorkerFactory<TFactory, TWorker, TAction>
where
    TFactory: WorkerFactory<TAction, Worker = TWorker>,
    TWorker: Worker<TAction> + Send + 'static,
    TAction: Action + Send + 'static,
{
    pub fn new(factory: TFactory) -> Self {
        Self {
            factory,
            _worker: Default::default(),
            _action: Default::default(),
        }
    }
}
impl<TFactory, TWorker, TAction> WorkerFactory<TAction>
    for BoxedWorkerFactory<TFactory, TWorker, TAction>
where
    TFactory: WorkerFactory<TAction, Worker = TWorker>,
    TWorker: Worker<TAction> + Send + 'static,
    TAction: Action + Send + 'static,
{
    type Worker = WorkerInstance<TAction>;
    fn create(&self) -> Self::Worker {
        Box::new(self.factory.create())
    }
}

pub type WorkerInstance<TAction> = Box<dyn Worker<TAction> + Send + 'static>;
impl<TAction: Action + Send + 'static> Worker<TAction> for WorkerInstance<TAction> {
    fn handle(
        &mut self,
        queue: WorkerMessageQueue<TAction>,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TAction> {
        let inner = &mut **self;
        inner.handle(queue, context)
    }
}

pub fn compose_actors<T: Action, T1: Actor<T>, T2: Actor<T>>(
    left: T1,
    right: T2,
) -> ChainedActor<T, T1, T2> {
    ChainedActor::new(left, right)
}

pub fn find_earliest_typed_message<'a, T: Action, TAction: Action + InboundAction<T>>(
    queue: &mut WorkerMessageQueue<TAction>,
) -> (Option<(T, MessageData)>, WorkerMessageQueue<TAction>) {
    let mut preceding_actions = WorkerMessageQueue::default();
    while let Some((action, metadata)) = queue.pop_front() {
        match action.try_into_type() {
            Ok(action) => {
                return (Some((action, metadata)), preceding_actions);
            }
            Err(action) => {
                preceding_actions.push_back((action, metadata));
            }
        }
    }
    (None, preceding_actions)
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
