// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::Named;
use reflex_macros::{blanket_trait, task_factory_enum, Matcher};

mod fixtures {
    use reflex_dispatcher::*;
    use reflex_macros::dispatcher;

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct FooMessage {
        pub foo: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct BarMessage<T: From<usize>> {
        pub bar: T,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct BazMessage {
        pub bar: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct QuuxMessage {
        pub quux: bool,
    }

    #[derive(Clone)]
    pub struct MyActor<'a, T>
    where
        T: From<usize>,
    {
        pub value: &'a T,
    }

    #[derive(Default)]
    pub struct MyActorState<T>
    where
        T: From<usize>,
    {
        pub value: T,
    }

    dispatcher!({
        pub enum MyActorAction<T: From<usize>> {
            Inbox(BarMessage<T>),
            Inbox(QuuxMessage),

            Outbox(FooMessage),
            Outbox(BazMessage),
        }

        impl<'a, T, TAction, TTask> Dispatcher<TAction, TTask> for MyActor<'a, T>
        where
            T: From<usize> + Default,
            TAction: Action,
            TTask: TaskFactory<TAction, TTask>,
        {
            type State = MyActorState<T>;
            type Events<TInbox: TaskInbox<TAction>> = TInbox;
            type Dispose = NoopDisposeCallback;

            fn init(&self) -> Self::State {
                Default::default()
            }

            fn events<TInbox: TaskInbox<TAction>>(
                &self,
                inbox: TInbox,
            ) -> ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
                ActorEvents::Sync(inbox)
            }
            fn accept(&self, _message: &BarMessage<T>) -> bool {
                true
            }
            fn schedule(
                &self,
                _message: &BarMessage<T>,
                _state: &Self::State,
            ) -> Option<SchedulerMode> {
                Some(SchedulerMode::Async)
            }
            fn handle(
                &self,
                state: &mut Self::State,
                message: &BarMessage<T>,
                metadata: &MessageData,
                context: &mut impl HandlerContext,
            ) -> Option<SchedulerTransition<TAction, TTask>> {
                self.handle_bar(state, message, metadata, context)
            }

            fn accept(&self, message: &QuuxMessage) -> bool {
                message.quux
            }
            fn schedule(
                &self,
                _message: &QuuxMessage,
                _state: &Self::State,
            ) -> Option<SchedulerMode> {
                Some(SchedulerMode::Async)
            }
            fn handle(
                &self,
                state: &mut Self::State,
                message: &QuuxMessage,
                metadata: &MessageData,
                context: &mut impl HandlerContext,
            ) -> Option<SchedulerTransition<TAction, TTask>> {
                self.handle_quux(state, message, metadata, context)
            }
        }
    });

    impl<'a, T> MyActor<'a, T>
    where
        T: From<usize>,
    {
        fn handle_bar<TAction, TTask>(
            &self,
            _state: &mut MyActorState<T>,
            _message: &BarMessage<T>,
            _metadata: &MessageData,
            _context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>>
        where
            TAction: Action + From<FooMessage>,
            TTask: TaskFactory<TAction, TTask>,
        {
            Some(SchedulerTransition::from_iter([SchedulerCommand::Send(
                ProcessId::default(),
                FooMessage::default().into(),
            )]))
        }
        fn handle_quux<TAction, TTask>(
            &self,
            _state: &mut MyActorState<T>,
            _message: &QuuxMessage,
            _metadata: &MessageData,
            _context: &mut impl HandlerContext,
        ) -> Option<SchedulerTransition<TAction, TTask>>
        where
            TAction: Action + From<BazMessage>,
            TTask: TaskFactory<TAction, TTask>,
        {
            Some(SchedulerTransition::from_iter([SchedulerCommand::Send(
                ProcessId::default(),
                BazMessage::default().into(),
            )]))
        }
    }

    impl<'a, T> Named for MyActor<'a, T>
    where
        T: From<usize>,
    {
        fn name(&self) -> &'static str {
            "name"
        }
    }

    pub struct MyActorFactory<'a, T>
    where
        T: From<usize>,
    {
        value: &'a T,
    }

    impl<'a, T, TAction, TTask> TaskFactory<TAction, TTask> for MyActorFactory<'a, T>
    where
        TTask: TaskFactory<TAction, TTask>,
        TAction: MyActorAction<T> + Action,
        T: From<usize> + Default,
    {
        type Actor = MyActor<'a, T>;

        fn create(self) -> Self::Actor {
            MyActor {
                value: self.value.clone(),
            }
        }
    }
    impl<'a, T> Named for MyActorFactory<'a, T>
    where
        T: From<usize>,
    {
        fn name(&self) -> &'static str {
            "factory"
        }
    }
}

#[test]
fn basic_usage() {
    use fixtures::*;
    use reflex_dispatcher::{Action, TaskFactory};
    blanket_trait!(
        pub trait MyAction<T: From<usize>>: MyActorAction<T> {}
    );
    task_factory_enum!({
        #[derive(Matcher)]
        pub enum MyFactory<'a, T>
        where
            T: From<usize> + Default + 'a,
        {
            MyActor(MyActorFactory<'a, T>),
        }
        impl<'a, T, TAction, TTask> TaskFactory<TAction, TTask> for MyFactory<'a, T>
        where
            T: From<usize> + Default + 'a,
            TAction: MyAction<T> + Action,
            TTask: TaskFactory<TAction, TTask>,
        {
        }
    });

    fn assert_named<T: Named>() {}
    assert_named::<MyFactory<'static, usize>>();
    assert_eq!(1, 1);
}
