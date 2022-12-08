// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde_json::{Map as JsonMap, Value as JsonValue};

pub fn json_object(properties: impl IntoIterator<Item = (String, JsonValue)>) -> JsonValue {
    JsonValue::Object(JsonMap::from_iter(properties))
}

pub fn is_empty_json_object(value: &JsonValue) -> bool {
    match value {
        JsonValue::Object(value) => value.is_empty(),
        _ => false,
    }
}
