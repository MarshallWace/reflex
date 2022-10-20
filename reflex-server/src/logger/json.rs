// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::marker::PhantomData;

use chrono::{DateTime, Duration, SecondsFormat, Utc};
use reflex_dispatcher::{
    Action, MessageData, MiddlewareContext, ProcessId, SchedulerCommand, SerializableAction,
    TaskFactory,
};
use reflex_json::{JsonMap, JsonValue};

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
    type Task = TTask;
    fn log(
        &mut self,
        action: &SchedulerCommand<Self::Action, Self::Task>,
        metadata: Option<&MessageData>,
        context: Option<&MiddlewareContext>,
    ) {
        let serialized_message = match action {
            SchedulerCommand::Send(pid, action) => Some(serialize_action_operation(
                action,
                *pid,
                metadata,
                context,
                &self.startup_time,
            )),
            _ => None,
        };
        if let Some(serialized_message) = serialized_message {
            let _ = writeln!(self.output, "{}", serialized_message.to_string());
        }
    }
}

fn serialize_action_operation<TAction>(
    action: &TAction,
    pid: ProcessId,
    metadata: Option<&MessageData>,
    context: Option<&MiddlewareContext>,
    startup_time: &StartupTime,
) -> JsonValue
where
    TAction: Action + JsonLoggerAction,
{
    JsonValue::Object(JsonMap::from_iter([
        (String::from("pid"), JsonValue::from(usize::from(pid))),
        (
            String::from("offset"),
            match metadata {
                Some(metadata) => JsonValue::from(usize::from(metadata.offset)),
                None => JsonValue::Null,
            },
        ),
        (
            String::from("timestamp"),
            match metadata.and_then(|metadata| startup_time.timestamp(metadata.timestamp)) {
                Some(timestamp) => {
                    JsonValue::from(timestamp.to_rfc3339_opts(SecondsFormat::Millis, true))
                }
                None => JsonValue::Null,
            },
        ),
        (
            String::from("caller_pid"),
            match context.and_then(|context| context.caller_pid) {
                Some(pid) => JsonValue::from(usize::from(pid)),
                None => JsonValue::Null,
            },
        ),
        (
            String::from("parent_offset"),
            match metadata.and_then(|metadata| metadata.parent) {
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
