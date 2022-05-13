// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use chrono::{DateTime, Duration, SecondsFormat, Utc};
use reflex_dispatcher::{Action, HandlerContext, MessageData, SerializableAction};
use reflex_json::{JsonMap, JsonValue};

use crate::{logger::ActionLogger, utils::sanitize::sanitize_json_value};

pub use chrono;

pub trait JsonLoggerAction: Action + SerializableAction {}
impl<TAction> JsonLoggerAction for TAction where Self: Action + SerializableAction {}

pub struct JsonActionLogger<TOut: std::io::Write> {
    startup_time: StartupTime,
    output: TOut,
}
impl<T: std::io::Write> JsonActionLogger<T> {
    pub fn new(output: T) -> Self {
        Self {
            output,
            startup_time: StartupTime::default(),
        }
    }
}
impl JsonActionLogger<std::io::Stdout> {
    pub fn stdout() -> Self {
        Self::new(std::io::stdout())
    }
}
impl Clone for JsonActionLogger<std::io::Stdout> {
    fn clone(&self) -> Self {
        Self {
            startup_time: self.startup_time,
            output: std::io::stdout(),
        }
    }
}
impl JsonActionLogger<std::io::Stderr> {
    pub fn stderr() -> Self {
        Self::new(std::io::stderr())
    }
}
impl Clone for JsonActionLogger<std::io::Stderr> {
    fn clone(&self) -> Self {
        Self {
            startup_time: self.startup_time,
            output: std::io::stderr(),
        }
    }
}
impl<TOut: std::io::Write, TAction: JsonLoggerAction> ActionLogger<TAction>
    for JsonActionLogger<TOut>
{
    fn log(
        &mut self,
        action: &TAction,
        metadata: Option<&MessageData>,
        context: Option<&impl HandlerContext>,
    ) {
        let serialized_message = JsonValue::Object(JsonMap::from_iter([
            (
                String::from("pid"),
                match context {
                    Some(context) => JsonValue::from(usize::from(context.pid())),
                    None => JsonValue::Null,
                },
            ),
            (
                String::from("offset"),
                match metadata {
                    Some(metadata) => JsonValue::from(usize::from(metadata.offset)),
                    None => JsonValue::Null,
                },
            ),
            (
                String::from("timestamp"),
                match metadata.and_then(|metadata| self.startup_time.timestamp(metadata.timestamp))
                {
                    Some(timestamp) => {
                        JsonValue::from(timestamp.to_rfc3339_opts(SecondsFormat::Millis, true))
                    }
                    None => JsonValue::Null,
                },
            ),
            (
                String::from("caller_pid"),
                match context.and_then(|context| context.caller_pid()) {
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
                sanitize_json_value(JsonValue::Object(JsonMap::from_iter(action.serialize()))),
            ),
        ]));
        let _ = writeln!(self.output, "{}", serialized_message.to_string());
    }
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
