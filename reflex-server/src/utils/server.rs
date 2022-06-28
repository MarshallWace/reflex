// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use futures::{Future, FutureExt, StreamExt};
use http::{Request, Response, StatusCode};
use hyper::{body, Body};
use reflex_dispatcher::{Action, AsyncScheduler};
use uuid::Uuid;

use crate::server::utils::create_http_response;

pub fn handle_http_request<TScheduler, TAction>(
    request: Request<Body>,
    scheduler: &TScheduler,
    create_request: impl Fn(Uuid, Request<Bytes>) -> TAction + Send + 'static,
    create_response: impl Fn(Uuid, Response<Bytes>) -> TAction + Send + 'static,
    match_result: impl for<'a> Fn(Uuid, &'a TAction) -> Option<Response<Bytes>> + Send + 'static,
) -> impl Future<Output = Response<Body>>
where
    TScheduler: AsyncScheduler<Action = TAction>,
    TAction: Action + Clone + Send + 'static,
{
    let (headers, body) = request.into_parts();
    let request_id = Uuid::new_v4();
    let dispatch_request = scheduler.dispatch(Box::pin(
        body::to_bytes(body)
            .map(move |bytes| match bytes {
                Ok(bytes) => create_request(request_id, Request::from_parts(headers, bytes)),
                Err(err) => create_response(
                    request_id,
                    create_http_response(
                        StatusCode::BAD_REQUEST,
                        None,
                        Some(format!("Failed to parse incoming request: {}", err)),
                    ),
                ),
            })
            .into_stream(),
    ));
    let subscribe_response_stream = scheduler
        .subscribe(move |action: &TAction| match_result(request_id, action))
        .map(|stream| stream.take(1));
    subscribe_response_stream.then({
        |response_stream| {
            dispatch_request.then(|_| {
                response_stream.into_future().map(|(response, _)| {
                    response
                        .map(|response| response.map(Body::from))
                        .unwrap_or_else(|| {
                            Response::builder()
                                .status(StatusCode::SERVICE_UNAVAILABLE)
                                .body(Default::default())
                                .unwrap()
                        })
                })
            })
        }
    })
}
