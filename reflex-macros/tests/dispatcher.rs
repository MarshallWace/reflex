// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::*;
use reflex_macros::dispatcher;

mod fixtures {
    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct FooMessage {
        pub foo: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct BarMessage {
        pub bar: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct BazMessage {
        pub bar: bool,
    }

    #[derive(PartialEq, Eq, Default, Debug)]
    pub struct QuuxMessage {
        pub quux: bool,
    }
}

#[test]
fn dispatcher() {
    use crate::fixtures::*;

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
        pub enum MyActorAction {
            Inbox(BarMessage),
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

            fn accept(&self, _message: &BarMessage) -> bool {
                true
            }
            fn schedule(
                &self,
                _message: &BarMessage,
                _state: &Self::State,
            ) -> Option<SchedulerMode> {
                Some(SchedulerMode::Async)
            }
            fn handle(
                &self,
                state: &mut Self::State,
                message: &BarMessage,
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
            _message: &BarMessage,
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
}
