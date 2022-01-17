// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_json::{JsonMap, JsonValue};

fn is_sanitized_json_field(key: &str) -> bool {
    // https://github.com/elastic/apm/blob/main/specs/agents/sanitization.md
    match key.to_ascii_lowercase().as_str() {
        "password" | "passwd" | "pwd" | "secret" | "authorization" | "set-cookie" => true,
        key => {
            key.ends_with("key")
                || key.contains("token")
                || key.contains("session")
                || key.contains("credit")
                || key.contains("card")
        }
    }
}

pub fn sanitize_json_value(value: JsonValue) -> JsonValue {
    match value {
        JsonValue::Object(value) => JsonValue::Object(JsonMap::from_iter(
            value.into_iter().map(sanitize_json_property),
        )),
        JsonValue::Array(value) => {
            JsonValue::Array(value.into_iter().map(sanitize_json_value).collect())
        }
        value => value,
    }
}

fn sanitize_json_property(property: (String, JsonValue)) -> (String, JsonValue) {
    let (key, value) = property;
    let sanitized_value = if is_sanitized_json_field(&key) {
        JsonValue::String("[REDACTED]".into())
    } else {
        sanitize_json_value(value)
    };
    (key, sanitized_value)
}
