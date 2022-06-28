// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use opentelemetry::{
    sdk,
    trace::{IdGenerator, SpanId, TraceFlags, TraceId},
};
use serde::{Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Traceparent {
    pub trace_id: TraceId,
    pub span_id: SpanId,
}
impl Traceparent {
    pub fn generate() -> Self {
        let generator = sdk::trace::IdGenerator::default();
        Self {
            trace_id: generator.new_trace_id(),
            span_id: generator.new_span_id(),
        }
    }
    pub fn generate_child(&self) -> Self {
        let generator = sdk::trace::IdGenerator::default();
        Self {
            trace_id: self.trace_id,
            span_id: generator.new_span_id(),
        }
    }
}
impl std::fmt::Display for Traceparent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // https://www.w3.org/TR/trace-context/#traceparent-header
        let version = "00";
        let flags = "01";
        write!(
            f,
            "{}-{}-{}-{}",
            version, self.trace_id, self.span_id, flags
        )
    }
}
impl serde::Serialize for Traceparent {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        SerializedTraceparent::from(self).serialize(serializer)
    }
}
impl<'de> serde::Deserialize<'de> for Traceparent {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SerializedTraceparent::deserialize(deserializer).map(Into::into)
    }
}
#[derive(Debug, Serialize, Deserialize)]
struct SerializedTraceparent {
    trace_id: [u8; 16],
    span_id: [u8; 8],
}
impl<'a> From<&'a Traceparent> for SerializedTraceparent {
    fn from(value: &'a Traceparent) -> Self {
        let Traceparent { trace_id, span_id } = value;
        SerializedTraceparent {
            trace_id: trace_id.clone().to_bytes(),
            span_id: span_id.clone().to_bytes(),
        }
    }
}
impl From<SerializedTraceparent> for Traceparent {
    fn from(value: SerializedTraceparent) -> Self {
        let SerializedTraceparent { trace_id, span_id } = value;
        Self {
            trace_id: TraceId::from_bytes(trace_id),
            span_id: SpanId::from_bytes(span_id),
        }
    }
}

pub fn parse_traceparent(value: &str) -> Option<Traceparent> {
    let ((_version, trace_id, span_id), remaining) = parse_traceparent_segments(value)?;
    if remaining.is_empty() {
        Some(Traceparent { trace_id, span_id })
    } else {
        None
    }
}

fn parse_traceparent_segments(input: &str) -> Option<((TraceFlags, TraceId, SpanId), &str)> {
    let remaining = input;
    let (version, remaining) = parse_trace_flags(remaining)?;
    let remaining = parse_hyphen(remaining)?;
    let (trace_id, remaining) = parse_trace_id(remaining)?;
    let remaining = parse_hyphen(remaining)?;
    let (span_id, remaining) = parse_span_id(remaining)?;
    Some(((version, trace_id, span_id), remaining))
}

fn parse_trace_flags(input: &str) -> Option<(TraceFlags, &str)> {
    parse_hex_u8(input).map(|(value, remaining)| (TraceFlags::new(value), remaining))
}

fn parse_trace_id(input: &str) -> Option<(TraceId, &str)> {
    let value = TraceId::from_hex(input.get(0..32)?).ok()?;
    Some((value, &input[32..]))
}

fn parse_span_id(input: &str) -> Option<(SpanId, &str)> {
    let value = SpanId::from_hex(input.get(0..16)?).ok()?;
    Some((value, &input[16..]))
}

fn parse_hyphen(input: &str) -> Option<&str> {
    match input.get(0..1)? {
        "-" => Some(&input[1..]),
        _ => None,
    }
}

fn parse_hex_u8(input: &str) -> Option<(u8, &str)> {
    let value = input.get(0..2)?.parse::<u8>().ok()?;
    Some((value, &input[2..]))
}
