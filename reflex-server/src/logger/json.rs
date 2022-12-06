// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{marker::PhantomData, ops::Deref, time::Instant};

use chrono::{DateTime, Duration, SecondsFormat, Utc};
use reflex_dispatcher::{Action, MessageOffset, ProcessId, SerializableAction, TaskFactory};
use reflex_json::{JsonMap, JsonValue};
use reflex_scheduler::tokio::{AsyncMessage, TokioCommand, TokioSchedulerLogger};

use crate::{logger::ActionLogger, utils::sanitize::sanitize_json_value};

pub use chrono;

pub trait JsonLoggerAction: SerializableAction {}
impl<_Self> JsonLoggerAction for _Self where Self: SerializableAction {}

pub struct JsonActionLogger<TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    startup_time: StartupTime,
    output: TOut,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TOut, TAction, TTask> JsonActionLogger<TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(output: TOut) -> Self {
        Self {
            output,
            startup_time: StartupTime::default(),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
    fn log(
        &mut self,
        action: &TAction,
        pid: Option<ProcessId>,
        offset: Option<MessageOffset>,
        timestamp: Option<std::time::Instant>,
        caller_pid: Option<ProcessId>,
        caller_offset: Option<MessageOffset>,
    ) {
        let serialized_message = serialize_action_operation(
            action,
            pid,
            offset,
            timestamp.map(|timestamp| (self.startup_time, timestamp)),
            caller_pid,
            caller_offset,
        );
        let _ = writeln!(self.output, "{}", serialized_message.to_string());
    }
}
impl<TAction, TTask> JsonActionLogger<std::io::Stdout, TAction, TTask>
where
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn stdout() -> Self {
        Default::default()
    }
}
impl<TAction, TTask> Default for JsonActionLogger<std::io::Stdout, TAction, TTask>
where
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    fn default() -> Self {
        Self::new(std::io::stdout())
    }
}
impl<TAction, TTask> Clone for JsonActionLogger<std::io::Stdout, TAction, TTask>
where
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self {
            startup_time: self.startup_time,
            output: std::io::stdout(),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TAction, TTask> JsonActionLogger<std::io::Stderr, TAction, TTask>
where
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn stderr() -> Self {
        Default::default()
    }
}
impl<TAction, TTask> Default for JsonActionLogger<std::io::Stderr, TAction, TTask>
where
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    fn default() -> Self {
        Self::new(std::io::stderr())
    }
}
impl<TAction, TTask> Clone for JsonActionLogger<std::io::Stderr, TAction, TTask>
where
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self {
            startup_time: self.startup_time,
            output: std::io::stderr(),
            _action: PhantomData,
            _task: PhantomData,
        }
    }
}
impl<TOut, TAction, TTask> ActionLogger for JsonActionLogger<TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    fn log(&mut self, action: &Self::Action) {
        JsonActionLogger::log(self, action, None, None, None, None, None)
    }
}
impl<TOut, TAction, TTask> TokioSchedulerLogger for JsonActionLogger<TOut, TAction, TTask>
where
    TOut: std::io::Write,
    TAction: Action + JsonLoggerAction,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;
    fn log_worker_message(
        &mut self,
        _message: &AsyncMessage<Self::Action>,
        _actor: &<Self::Task as TaskFactory<Self::Action, Self::Task>>::Actor,
        _pid: ProcessId,
    ) {
    }
    fn log_task_message(&mut self, _message: &AsyncMessage<Self::Action>, _pid: ProcessId) {}
    fn log_scheduler_command(
        &mut self,
        command: &TokioCommand<Self::Action, Self::Task>,
        _enqueue_time: Instant,
    ) {
        match command {
            TokioCommand::Send { pid, message } => {
                let caller = message.caller().and_then(|caller| caller);
                let caller_pid = caller.map(|(_, caller_pid)| caller_pid);
                let caller_offset = caller.map(|(caller_offset, _)| caller_offset);
                let action = message.deref();
                let offset = message.offset();
                let timestamp = message.enqueue_time();
                JsonActionLogger::log(
                    self,
                    &action,
                    Some(*pid),
                    offset,
                    timestamp,
                    caller_pid,
                    caller_offset,
                );
            }
            _ => {}
        }
    }
}

fn serialize_action_operation<TAction>(
    action: &TAction,
    pid: Option<ProcessId>,
    offset: Option<MessageOffset>,
    timestamp: Option<(StartupTime, Instant)>,
    caller_pid: Option<ProcessId>,
    caller_offset: Option<MessageOffset>,
) -> JsonValue
where
    TAction: Action + JsonLoggerAction,
{
    JsonValue::Object(JsonMap::from_iter([
        (
            String::from("pid"),
            match pid {
                Some(pid) => JsonValue::from(usize::from(pid)),
                None => JsonValue::Null,
            },
        ),
        (
            String::from("offset"),
            match offset {
                Some(offset) => JsonValue::from(usize::from(offset)),
                None => JsonValue::Null,
            },
        ),
        (
            String::from("timestamp"),
            match timestamp.and_then(|(startup_time, timestamp)| startup_time.timestamp(timestamp))
            {
                Some(timestamp) => {
                    JsonValue::from(timestamp.to_rfc3339_opts(SecondsFormat::Millis, true))
                }
                None => JsonValue::Null,
            },
        ),
        (
            String::from("caller_pid"),
            match caller_pid {
                Some(pid) => JsonValue::from(usize::from(pid)),
                None => JsonValue::Null,
            },
        ),
        (
            String::from("parent_offset"),
            match caller_offset {
                Some(offset) => JsonValue::from(usize::from(offset)),
                None => JsonValue::Null,
            },
        ),
        (
            String::from("event"),
            JsonValue::String(String::from(action.name())),
        ),
        (
            String::from("args"),
            sanitize_json_value(JsonValue::Object(JsonMap::from_iter(action.to_json()))),
        ),
    ]))
}

#[derive(Clone, Copy, Debug)]
pub struct StartupTime {
    system_time: DateTime<Utc>,
    instant: std::time::Instant,
}
impl Default for StartupTime {
    fn default() -> Self {
        Self {
            system_time: Utc::now(),
            instant: std::time::Instant::now(),
        }
    }
}
impl StartupTime {
    pub fn timestamp(&self, instant: std::time::Instant) -> Option<DateTime<Utc>> {
        let duration = Duration::from_std(instant.duration_since(self.instant)).ok()?;
        self.system_time.checked_add_signed(duration)
    }
}
