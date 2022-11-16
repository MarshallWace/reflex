// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{marker::PhantomData, ops::Deref, pin::Pin};

use futures::{future::AbortHandle, Future, Stream};
use serde::{Deserialize, Serialize};
use serde_json::{Map as JsonMap, Value as JsonValue};

mod actor;
pub mod utils;

pub use actor::*;

pub trait Matcher<T>
where
    Self: Sized,
{
    fn match_type(&self) -> Option<&T>;
}
impl<_Self, T> Matcher<T> for _Self
where
    for<'a> Option<&'a T>: From<&'a Self>,
{
    fn match_type(&self) -> Option<&T> {
        self.into()
    }
}

pub trait Action {}

pub trait Named {
    fn name(&self) -> &'static str;
}

pub trait SerializableAction: Named {
    fn to_json(&self) -> SerializedAction;
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

pub trait Actor<TAction: Action, TTask: TaskFactory<TAction, TTask>>:
    Worker<TAction, SchedulerTransition<TAction, TTask>>
{
    type Events<TInbox: TaskInbox<TAction>>: Stream<Item = TInbox::Message> + Unpin + Send;
    type Dispose: Future<Output = ()> + Unpin;
    fn init<TInbox: TaskInbox<TAction>>(
        &self,
        inbox: TInbox,
        context: &impl ActorInitContext,
    ) -> (Self::State, Self::Events<TInbox>, Self::Dispose);
}

pub trait ActorInitContext: 'static {
    fn pid(&self) -> ProcessId;
}

pub trait Handler<I, O> {
    type State;
    fn handle(
        &self,
        state: &mut Self::State,
        message: &I,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> Option<O>;
}

pub trait Worker<I, O>: Handler<I, O> {
    fn accept(&self, message: &I) -> bool;
    fn schedule(&self, message: &I, state: &Self::State) -> Option<SchedulerMode>;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum SchedulerMode {
    Sync,
    Async,
    Blocking,
}

pub trait TaskFactory<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    type Actor: Actor<TAction, TTask>;
    fn create(self) -> Self::Actor;
}

pub trait TaskInbox<TAction: Action>:
    Stream<Item = Self::Message> + Unpin + Send + 'static
{
    type Message: TaskMessage<TAction>;
}

pub trait TaskMessage<TAction: Action>: From<TAction> + Deref<Target = TAction> {}

pub type BoxedActionStream<T> = Pin<Box<dyn Stream<Item = T> + Send + 'static>>;

#[derive(Default)]
pub struct NoopDisposeCallback;
impl Future for NoopDisposeCallback {
    type Output = ();
    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        std::task::Poll::Ready(())
    }
}

pub struct TaskAbortHandle(AbortHandle);
impl From<AbortHandle> for TaskAbortHandle {
    fn from(value: AbortHandle) -> Self {
        Self(value)
    }
}
impl Future for TaskAbortHandle {
    type Output = ();
    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.0.abort();
        std::task::Poll::Ready(())
    }
}

pub struct SchedulerMiddleware<TPre, TPost, TAction, TTask> {
    pub pre: TPre,
    pub post: TPost,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TPre, TPost, TAction, TTask> Clone for SchedulerMiddleware<TPre, TPost, TAction, TTask>
where
    TPre: Clone,
    TPost: Clone,
{
    fn clone(&self) -> Self {
        Self {
            pre: self.pre.clone(),
            post: self.post.clone(),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TPre, TPost, TAction: Action, TTask: TaskFactory<TAction, TTask>>
    SchedulerMiddleware<TPre, TPost, TAction, TTask>
where
    TPre: PreMiddleware<TAction, TTask>,
    TPost: PostMiddleware<TAction, TTask>,
{
    pub fn new(pre: TPre, post: TPost) -> Self {
        Self {
            pre,
            post,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
    pub fn into_parts(self) -> (TPre, TPost) {
        let Self { pre, post, .. } = self;
        (pre, post)
    }
}
impl<TPre, TAction: Action, TTask: TaskFactory<TAction, TTask>>
    SchedulerMiddleware<TPre, NoopActor, TAction, TTask>
where
    TPre: PreMiddleware<TAction, TTask>,
{
    pub fn pre(pre: TPre) -> Self {
        Self {
            pre,
            post: NoopActor,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TPost, TAction: Action, TTask: TaskFactory<TAction, TTask>>
    SchedulerMiddleware<NoopActor, TPost, TAction, TTask>
where
    TPost: PostMiddleware<TAction, TTask>,
{
    pub fn post(post: TPost) -> Self {
        Self {
            pre: NoopActor,
            post,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction: Action, TTask: TaskFactory<TAction, TTask>>
    SchedulerMiddleware<NoopActor, NoopActor, TAction, TTask>
{
    pub fn noop() -> Self {
        Self {
            pre: NoopActor,
            post: NoopActor,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}

pub trait PreMiddleware<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    type State;
    fn init(&self) -> Self::State;
    fn handle(
        &self,
        state: &mut Self::State,
        operation: SchedulerCommand<TAction, TTask>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> SchedulerCommand<TAction, TTask>;
}

pub trait PostMiddleware<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    type State;
    fn init(&self) -> Self::State;
    fn handle(
        &self,
        state: &mut Self::State,
        operations: Vec<SchedulerCommand<TAction, TTask>>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> Option<SchedulerTransition<TAction, TTask>>;
}

#[derive(Clone, Copy, Debug)]
pub struct MiddlewareContext {
    pub caller_pid: Option<ProcessId>,
}

#[derive(Clone, Copy, Debug)]
pub struct MessageData {
    pub offset: MessageOffset,
    pub parent: Option<MessageOffset>,
    pub timestamp: std::time::Instant,
}

#[derive(
    PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default, Debug, Serialize, Deserialize, Hash,
)]
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

pub struct HandlerTransition<T>(Vec<T>);
impl<T> Default for HandlerTransition<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<T> HandlerTransition<T> {
    pub fn new(messages: impl IntoIterator<Item = T>) -> Self {
        Self::from_iter(messages)
    }
    pub fn len(&self) -> usize {
        let Self(messages) = self;
        messages.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn append(self, other: HandlerTransition<T>) -> HandlerTransition<T> {
        let Self(mut operations) = self;
        let Self(other_operations) = other;
        operations.extend(other_operations);
        Self(operations)
    }
    pub fn into_inner(self) -> Vec<T> {
        let Self(operations) = self;
        operations
    }
}
impl<T> FromIterator<T> for HandlerTransition<T> {
    fn from_iter<TIter: IntoIterator<Item = T>>(iter: TIter) -> Self {
        Self(iter.into_iter().collect())
    }
}
impl<T> FromIterator<HandlerTransition<T>> for HandlerTransition<T> {
    fn from_iter<TIter: IntoIterator<Item = HandlerTransition<T>>>(iter: TIter) -> Self {
        Self(
            iter.into_iter()
                .flat_map(|item| item.into_inner())
                .collect(),
        )
    }
}
impl<T> IntoIterator for HandlerTransition<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        let Self(actions) = self;
        actions.into_iter()
    }
}

pub type SchedulerTransition<TAction, TTask> = HandlerTransition<SchedulerCommand<TAction, TTask>>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SchedulerCommand<TAction: Action, TTask: TaskFactory<TAction, TTask>> {
    Forward(ProcessId),
    Send(ProcessId, TAction),
    Task(ProcessId, TTask),
    Kill(ProcessId),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Default, Debug, Serialize, Deserialize)]
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
    fn generate_pid(&mut self) -> ProcessId;
}
