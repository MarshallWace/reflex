// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use hyper::{Body, Request, Uri};
use hyper_tls::HttpsConnector;

pub(crate) async fn fetch(
    method: String,
    url: String,
    headers: impl IntoIterator<Item = (String, String)>,
    body: Option<String>,
) -> Result<(u16, String), String> {
    let https = HttpsConnector::new();
    let client = hyper::Client::builder().build::<_, hyper::Body>(https);
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
                    Ok(result) => {
                        let status = result.status().as_u16();
                        match hyper::body::to_bytes(result.into_body()).await {
                            Ok(body) => match String::from_utf8(body.into_iter().collect()) {
                                Ok(data) => Ok((status, data)),
                                Err(error) => Err(format!("{}", error)),
                            },
                            Err(error) => Err(format!("{}", error)),
                        }
                    }
                    Err(error) => Err(format!("{}", error)),
                },
                Err(error) => Err(format!("{}", error)),
            }
        }
        Err(error) => Err(format!("{}", error)),
    }
}
