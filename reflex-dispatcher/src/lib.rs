// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{iter::empty, marker::PhantomData, pin::Pin, rc::Rc, sync::Arc};

use futures::{
    future::{self, AbortHandle, Abortable},
    stream, Future, Stream,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{Map as JsonMap, Value as JsonValue};

mod actor;
pub mod scheduler;
pub mod session_playback;
pub mod session_recorder;
pub mod utils;

pub use actor::*;
pub use session_playback::*;
pub use session_recorder::*;

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

pub trait Actor<TAction: Action> {
    type State;
    fn init(&self) -> Self::State;
    fn handle(
        &self,
        state: Self::State,
        action: &TAction,
        metadata: &MessageData,
        context: &mut impl HandlerContext,
    ) -> ActorTransition<Self::State, TAction>;
}

pub struct SchedulerMiddleware<TPre, TPost, TAction> {
    pub pre: TPre,
    pub post: TPost,
    _action: PhantomData<TAction>,
}
impl<TPre, TPost, TAction> Clone for SchedulerMiddleware<TPre, TPost, TAction>
where
    TPre: Clone,
    TPost: Clone,
{
    fn clone(&self) -> Self {
        Self {
            pre: self.pre.clone(),
            post: self.post.clone(),
            _action: Default::default(),
        }
    }
}
impl<TPre, TPost, TAction: Action> SchedulerMiddleware<TPre, TPost, TAction>
where
    TPre: PreMiddleware<TAction>,
    TPost: PostMiddleware<TAction>,
{
    pub fn new(pre: TPre, post: TPost) -> Self {
        Self {
            pre,
            post,
            _action: Default::default(),
        }
    }
    pub fn into_parts(self) -> (TPre, TPost) {
        let Self { pre, post, _action } = self;
        (pre, post)
    }
}
impl<TPre, TAction: Action> SchedulerMiddleware<TPre, NoopActor, TAction>
where
    TPre: PreMiddleware<TAction>,
{
    pub fn pre(pre: TPre) -> Self {
        Self {
            pre,
            post: NoopActor,
            _action: Default::default(),
        }
    }
}
impl<TPost, TAction: Action> SchedulerMiddleware<NoopActor, TPost, TAction>
where
    TPost: PostMiddleware<TAction>,
{
    pub fn post(post: TPost) -> Self {
        Self {
            pre: NoopActor,
            post,
            _action: Default::default(),
        }
    }
}
impl<TAction: Action> SchedulerMiddleware<NoopActor, NoopActor, TAction> {
    pub fn noop() -> Self {
        Self {
            pre: NoopActor,
            post: NoopActor,
            _action: Default::default(),
        }
    }
}

pub trait PreMiddleware<TAction: Action> {
    type State;
    fn init(&self) -> Self::State;
    fn handle(
        &self,
        state: Self::State,
        operation: StateOperation<TAction>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PreMiddlewareTransition<Self::State, TAction>;
}

pub trait PostMiddleware<TAction: Action> {
    type State;
    fn init(&self) -> Self::State;
    fn handle(
        &self,
        state: Self::State,
        operations: Vec<StateOperation<TAction>>,
        metadata: &MessageData,
        context: &MiddlewareContext,
    ) -> PostMiddlewareTransition<Self::State, TAction>;
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

pub struct StateTransition<TAction: Action>(Vec<StateOperation<TAction>>);
impl<TAction: Action> Default for StateTransition<TAction> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<TAction: Action> StateTransition<TAction> {
    pub fn new(operations: impl IntoIterator<Item = StateOperation<TAction>>) -> Self {
        Self::from_iter(operations)
    }
    pub fn append(self, other: StateTransition<TAction>) -> StateTransition<TAction> {
        let Self(mut operations) = self;
        let Self(other_operations) = other;
        operations.extend(other_operations);
        Self(operations)
    }
    pub fn into_operations(self) -> Vec<StateOperation<TAction>> {
        let Self(operations) = self;
        operations
    }
}
impl<TAction: Action> FromIterator<StateOperation<TAction>> for StateTransition<TAction> {
    fn from_iter<T: IntoIterator<Item = StateOperation<TAction>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
impl<TAction: Action> IntoIterator for StateTransition<TAction> {
    type Item = StateOperation<TAction>;
    type IntoIter = std::vec::IntoIter<StateOperation<TAction>>;
    fn into_iter(self) -> Self::IntoIter {
        let Self(actions) = self;
        actions.into_iter()
    }
}

pub struct ActorTransition<S, TAction: Action> {
    state: S,
    actions: StateTransition<TAction>,
}
impl<S, TAction: Action> ActorTransition<S, TAction> {
    pub fn new(state: S, actions: StateTransition<TAction>) -> Self {
        Self { state, actions }
    }
    pub fn into_parts(self) -> (S, StateTransition<TAction>) {
        let Self { state, actions } = self;
        (state, actions)
    }
}

pub struct PreMiddlewareTransition<S, TAction: Action> {
    state: S,
    operation: StateOperation<TAction>,
}
impl<S, TAction: Action> PreMiddlewareTransition<S, TAction> {
    pub fn new(state: S, operation: StateOperation<TAction>) -> Self {
        Self { state, operation }
    }
    pub fn into_parts(self) -> (S, StateOperation<TAction>) {
        let Self { state, operation } = self;
        (state, operation)
    }
}

pub struct PostMiddlewareTransition<S, TAction: Action> {
    state: S,
    operations: Vec<StateOperation<TAction>>,
}
impl<S, TAction: Action> PostMiddlewareTransition<S, TAction> {
    pub fn new(state: S, operations: Vec<StateOperation<TAction>>) -> Self {
        Self { state, operations }
    }
    pub fn into_parts(self) -> (S, Vec<StateOperation<TAction>>) {
        let Self { state, operations } = self;
        (state, operations)
    }
}

pub enum StateOperation<TAction: Action> {
    Send(ProcessId, TAction),
    Task(ProcessId, OperationStream<TAction>),
    Spawn(ProcessId, BoxedWorkerFactory<TAction>),
    Kill(ProcessId),
}
impl<TAction> StateOperation<TAction>
where
    TAction: Action + Send + 'static,
{
    pub fn spawn(pid: ProcessId, worker: impl WorkerFactory<TAction>) -> Self {
        Self::Spawn(pid, BoxedWorkerFactory::new(worker))
    }
}

// FIXME: Support fully-serializable task/worker types to prevent losing information when serializing/deserializing/hydrating
// Required for session playback: operations need to be hydrated from recorded values
impl<'a, TAction: Action + Clone + Send + 'static> From<&'a StateOperation<TAction>>
    for StateOperation<TAction>
{
    fn from(value: &'a StateOperation<TAction>) -> Self {
        match value {
            StateOperation::Send(pid, action) => StateOperation::Send(*pid, action.clone()),
            StateOperation::Task(pid, _) => StateOperation::Task(*pid, OperationStream::noop()),
            StateOperation::Spawn(pid, _) => {
                StateOperation::Spawn(*pid, BoxedWorkerFactory::noop())
            }
            StateOperation::Kill(pid) => StateOperation::Kill(*pid),
        }
    }
}
// Required for file recorder: operations need to be serializable to / deserializable from the filesystem
impl<TAction: Action + Clone> Serialize for StateOperation<TAction>
where
    TAction: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        SerializedStateOperation::from(self).serialize(serializer)
    }
}
impl<'de, TAction: Action + Send + 'static> Deserialize<'de> for StateOperation<TAction>
where
    TAction: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        SerializedStateOperation::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Serialize, Deserialize)]
pub enum SerializedStateOperation<TAction> {
    Send(ProcessId, TAction),
    Task(ProcessId),
    Spawn(ProcessId),
    Kill(ProcessId),
}
impl<'a, TAction: Action + Clone> From<&'a StateOperation<TAction>>
    for SerializedStateOperation<TAction>
{
    fn from(value: &'a StateOperation<TAction>) -> Self {
        match value {
            StateOperation::Send(pid, action) => Self::Send(*pid, action.clone()),
            StateOperation::Task(pid, _) => Self::Task(*pid),
            StateOperation::Spawn(pid, _) => Self::Spawn(*pid),
            StateOperation::Kill(pid) => Self::Kill(*pid),
        }
    }
}
impl<TAction: Action + Send + 'static> From<SerializedStateOperation<TAction>>
    for StateOperation<TAction>
{
    fn from(value: SerializedStateOperation<TAction>) -> Self {
        match value {
            SerializedStateOperation::Send(pid, action) => Self::Send(pid, action),
            SerializedStateOperation::Task(pid) => Self::Task(pid, OperationStream::noop()),
            SerializedStateOperation::Spawn(pid) => Self::Spawn(pid, BoxedWorkerFactory::noop()),
            SerializedStateOperation::Kill(pid) => Self::Kill(pid),
        }
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

pub struct OperationStream<TAction: Action> {
    task: Box<dyn Stream<Item = StateOperation<TAction>> + Send + Unpin + 'static>,
    dispose: BoxedDisposeCallback,
}
impl<TAction> OperationStream<TAction>
where
    TAction: Action + Send + 'static,
{
    pub fn new(task: impl Stream<Item = StateOperation<TAction>> + Send + Unpin + 'static) -> Self {
        let (abort_handle, abort_registration) = AbortHandle::new_pair();
        let task = Abortable::new(task, abort_registration);
        Self {
            task: Box::new(task),
            dispose: BoxedDisposeCallback::new(abort_handle),
        }
    }
    pub fn disposable(
        task: impl Stream<Item = StateOperation<TAction>> + Send + Unpin + 'static,
        dispose: impl DisposeCallback,
    ) -> Self {
        Self {
            task: Box::new(task),
            dispose: BoxedDisposeCallback::new(dispose),
        }
    }
    pub fn noop() -> Self {
        Self {
            task: Box::new(stream::empty()),
            dispose: BoxedDisposeCallback::new(NoopDisposeCallback),
        }
    }
    pub(crate) fn from_parts(
        task: Box<dyn Stream<Item = StateOperation<TAction>> + Send + Unpin + 'static>,
        dispose: BoxedDisposeCallback,
    ) -> Self {
        Self { task, dispose }
    }
    pub(crate) fn into_parts(
        self,
    ) -> (
        Box<dyn Stream<Item = StateOperation<TAction>> + Send + Unpin + 'static>,
        BoxedDisposeCallback,
    ) {
        (self.task, self.dispose)
    }
}

pub trait Scheduler {
    type Action: Action;
}

pub trait AsyncScheduler: Scheduler {
    // TODO: refactor method return values into static associated types once GATs are stabilized
    fn dispatch<TActions: AsyncActionStream<Self::Action>>(
        &self,
        actions: TActions,
    ) -> AsyncDispatchResult;
    fn subscribe<V: Send + 'static, TFilter: AsyncActionFilter<Self::Action, V>>(
        &self,
        transform: TFilter,
    ) -> AsyncSubscriptionStream<V>;
}
impl<T: AsyncScheduler> Scheduler for Rc<T> {
    type Action = T::Action;
}
impl<T: AsyncScheduler> AsyncScheduler for Rc<T> {
    fn dispatch<TActions: AsyncActionStream<Self::Action>>(
        &self,
        actions: TActions,
    ) -> AsyncDispatchResult {
        self.as_ref().dispatch(actions)
    }
    fn subscribe<V: Send + 'static, TFilter: AsyncActionFilter<Self::Action, V>>(
        &self,
        transform: TFilter,
    ) -> AsyncSubscriptionStream<V> {
        self.as_ref().subscribe(transform)
    }
}
impl<T: AsyncScheduler> Scheduler for Arc<T> {
    type Action = T::Action;
}
impl<T: AsyncScheduler> AsyncScheduler for Arc<T> {
    fn dispatch<TActions: AsyncActionStream<Self::Action>>(
        &self,
        actions: TActions,
    ) -> AsyncDispatchResult {
        self.as_ref().dispatch(actions)
    }
    fn subscribe<V: Send + 'static, TFilter: AsyncActionFilter<Self::Action, V>>(
        &self,
        transform: TFilter,
    ) -> AsyncSubscriptionStream<V> {
        self.as_ref().subscribe(transform)
    }
}

pub trait AsyncActionStream<TAction: Action>:
    Stream<Item = TAction> + Unpin + Send + 'static
{
}
impl<T, TAction: Action> AsyncActionStream<TAction> for T where
    T: Stream<Item = TAction> + Unpin + Send + 'static
{
}

pub type AsyncDispatchResult = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;

pub trait AsyncActionFilter<TAction: Action, TResult>: Send + 'static {
    fn transform(&self, action: &TAction) -> Option<TResult>;
}
impl<T, TAction, TResult> AsyncActionFilter<TAction, TResult> for T
where
    TAction: Action,
    T: for<'a> Fn(&'a TAction) -> Option<TResult> + Send + 'static,
{
    fn transform(&self, action: &TAction) -> Option<TResult> {
        (self)(action)
    }
}

pub type AsyncSubscriptionStream<T> =
    Pin<Box<dyn Future<Output = Pin<Box<dyn Stream<Item = T> + Send + 'static>>> + Send + 'static>>;

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
    fn caller_pid(&self) -> Option<ProcessId>;
    fn generate_pid(&mut self) -> ProcessId;
}

pub trait Worker<TAction: Action>: Send + 'static {
    fn handle(
        &mut self,
        action: TAction,
        metadata: &MessageData,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TAction>;
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

pub struct WorkerTransition<TAction: Action> {
    operations: StateTransition<TAction>,
}
impl<TAction: Action> Default for WorkerTransition<TAction> {
    fn default() -> Self {
        Self {
            operations: Default::default(),
        }
    }
}
impl<TAction: Action> WorkerTransition<TAction> {
    pub fn new(operations: impl IntoIterator<Item = StateOperation<TAction>>) -> Self {
        Self {
            operations: StateTransition::new(operations),
        }
    }
    pub fn append(self, other: WorkerTransition<TAction>) -> WorkerTransition<TAction> {
        let WorkerTransition { operations } = self;
        let operations = operations.append(other.operations);
        WorkerTransition { operations }
    }
    pub fn into_inner(self) -> StateTransition<TAction> {
        let Self { operations } = self;
        operations
    }
}
impl<TAction: Action> IntoIterator for WorkerTransition<TAction> {
    type Item = <StateTransition<TAction> as IntoIterator>::Item;
    type IntoIter = <StateTransition<TAction> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.operations.into_iter()
    }
}

pub trait WorkerFactory<TAction: Action>: Send + 'static {
    type Worker: Worker<TAction>;
    fn create(&self) -> Self::Worker;
}
pub struct NoopWorkerFactory;
impl<TAction: Action> WorkerFactory<TAction> for NoopWorkerFactory {
    type Worker = NoopWorker;
    fn create(&self) -> Self::Worker {
        NoopWorker
    }
}
pub struct NoopWorker;
impl<TAction: Action> Worker<TAction> for NoopWorker {
    fn handle(
        &mut self,
        _action: TAction,
        _metadata: &MessageData,
        _context: &mut WorkerContext,
    ) -> WorkerTransition<TAction> {
        WorkerTransition::new(empty())
    }
}
impl<TWorker, TAction> WorkerFactory<TAction> for Box<dyn WorkerFactory<TAction, Worker = TWorker>>
where
    TWorker: Worker<TAction>,
    TAction: Action + Send + 'static,
{
    type Worker = BoxedWorkerInstance<TAction>;
    fn create(&self) -> Self::Worker {
        let inner = &**self;
        Box::new(inner.create())
    }
}
pub struct BoxedWorkerFactory<TAction: Action> {
    factory: Box<dyn WorkerFactory<TAction, Worker = BoxedWorkerInstance<TAction>>>,
}
impl<TAction> BoxedWorkerFactory<TAction>
where
    TAction: Action + Send + 'static,
{
    pub fn new<TFactory>(factory: TFactory) -> Self
    where
        TFactory: WorkerFactory<TAction>,
    {
        Self {
            factory: Box::new(BoxedWorkerFactoryWrapper::new(factory)),
        }
    }
}
impl<TAction> BoxedWorkerFactory<TAction>
where
    TAction: Action + Send + 'static,
{
    pub fn noop() -> BoxedWorkerFactory<TAction> {
        Self {
            factory: Box::new(BoxedNoopWorkerFactory),
        }
    }
}
struct BoxedWorkerFactoryWrapper<TFactory: WorkerFactory<TAction>, TAction: Action> {
    factory: TFactory,
    _action: PhantomData<TAction>,
}
impl<TAction, TFactory> BoxedWorkerFactoryWrapper<TFactory, TAction>
where
    TAction: Action,
    TFactory: WorkerFactory<TAction>,
{
    fn new(factory: TFactory) -> Self {
        Self {
            factory,
            _action: Default::default(),
        }
    }
}
impl<TAction> WorkerFactory<TAction> for BoxedWorkerFactory<TAction>
where
    TAction: Action + Send + 'static,
{
    type Worker = BoxedWorkerInstance<TAction>;
    fn create(&self) -> Self::Worker {
        Box::new(self.factory.create())
    }
}
impl<TFactory, TAction> WorkerFactory<TAction> for BoxedWorkerFactoryWrapper<TFactory, TAction>
where
    TFactory: WorkerFactory<TAction>,
    TAction: Action + Send + 'static,
{
    type Worker = BoxedWorkerInstance<TAction>;
    fn create(&self) -> Self::Worker {
        Box::new(self.factory.create())
    }
}
pub struct BoxedNoopWorkerFactory;
impl<TAction> WorkerFactory<TAction> for BoxedNoopWorkerFactory
where
    TAction: Action + Send + 'static,
{
    type Worker = BoxedWorkerInstance<TAction>;
    fn create(&self) -> Self::Worker {
        Box::new(NoopWorker)
    }
}

pub type BoxedWorkerInstance<TAction> = Box<dyn Worker<TAction>>;
impl<TAction> Worker<TAction> for BoxedWorkerInstance<TAction>
where
    TAction: Action + Send + 'static,
{
    fn handle(
        &mut self,
        action: TAction,
        metadata: &MessageData,
        context: &mut WorkerContext,
    ) -> WorkerTransition<TAction> {
        let inner = &mut **self;
        inner.handle(action, metadata, context)
    }
}
