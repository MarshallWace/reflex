// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use hyper::{Body, Request, Uri};

pub(crate) async fn fetch(
    method: String,
    url: String,
    headers: impl IntoIterator<Item = (String, String)>,
    body: Option<String>,
) -> Result<String, String> {
    let client = hyper::Client::new();
    match url.parse::<Uri>() {
        Ok(url) => {
            let method: &str = &method;
            let request = Request::builder().method(method).uri(url);
            let request = headers.into_iter().fold(request, |request, (key, value)| {
                request.header(&key, &value)
            });
            let body = Body::from(body.unwrap_or(String::new()));
            match request.body(body) {
                Ok(request) => match client.request(request).await {
                    Ok(result) => match hyper::body::to_bytes(result.into_body()).await {
                        Ok(body) => match String::from_utf8(body.into_iter().collect()) {
                            Ok(data) => Ok(data),
                            Err(error) => Err(format!("{}", error)),
                        },
                        Err(error) => Err(format!("{}", error)),
                    },
                    Err(error) => Err(format!("{}", error)),
                },
                Err(error) => Err(format!("{}", error)),
            }
        }
        Err(error) => Err(format!("{}", error)),
    }
}
