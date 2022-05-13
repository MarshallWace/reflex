// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use graphql_parser::query::Document;

use crate::{GraphQlExtensions, GraphQlQueryTransform, GraphQlText};

pub struct WithExtensionsGraphQlTransform {
    extensions: GraphQlExtensions,
}
impl WithExtensionsGraphQlTransform {
    pub fn new(extensions: GraphQlExtensions) -> Self {
        Self { extensions }
    }
}
impl GraphQlQueryTransform for WithExtensionsGraphQlTransform {
    fn transform<'src, T: GraphQlText<'src>>(
        &self,
        document: Document<'src, T>,
        extensions: GraphQlExtensions,
    ) -> Result<(Document<'src, T>, GraphQlExtensions), String> {
        let extensions = {
            let mut combined_extensions = extensions;
            combined_extensions.extend(
                self.extensions
                    .iter()
                    .map(|(key, value)| (key.clone(), value.clone())),
            );
            combined_extensions
        };
        Ok((document, extensions))
    }
}
