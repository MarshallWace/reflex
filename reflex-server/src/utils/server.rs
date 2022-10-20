// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use bytes::Bytes;
use futures::{future, Future, FutureExt, SinkExt, StreamExt};
use http::{Request, Response, StatusCode};
use hyper::{body, Body};
use reflex_dispatcher::{Action, ProcessId, TaskFactory};
use reflex_scheduler::tokio::TokioScheduler;
use uuid::Uuid;

use crate::server::utils::create_http_response;

pub fn handle_http_request<TAction, TTask>(
    request: Request<Body>,
    scheduler: &TokioScheduler<TAction, TTask>,
    main_pid: ProcessId,
    create_request: impl Fn(Uuid, Request<Bytes>) -> TAction + 'static,
    create_response: impl Fn(Uuid, Response<Bytes>) -> TAction + 'static,
    match_result: impl for<'a> Fn(Uuid, &'a TAction) -> Option<Response<Bytes>> + Send + 'static,
) -> impl Future<Output = Response<Body>> + 'static
where
    TAction: Action + Send + Sync + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
{
    let (headers, body) = request.into_parts();
    let request_id = Uuid::new_v4();
    let dispatch_request = {
        let mut actions = scheduler.actions();
        async move {
            let action = match body::to_bytes(body).await {
                Ok(bytes) => create_request(request_id, Request::from_parts(headers, bytes)),
                Err(err) => create_response(
                    request_id,
                    create_http_response(
                        StatusCode::BAD_REQUEST,
                        None,
                        Some(format!("Failed to parse incoming request: {}", err)),
                    ),
                ),
            };
            actions.send((main_pid, action)).await
        }
    };
    let subscribe_response_stream = scheduler
        .subscribe(main_pid, move |action: &TAction| {
            match_result(request_id, action)
        })
        .map(|stream| stream.take(1));
    subscribe_response_stream.then({
        |response_stream| {
            dispatch_request.then(|result| match result {
                Err(_) => future::ready(create_http_response(
                    StatusCode::INTERNAL_SERVER_ERROR,
                    None,
                    None,
                ))
                .left_future(),
                Ok(_) => Box::pin(response_stream)
                    .into_future()
                    .map(|(response, _)| {
                        response
                            .map(|response| response.map(Body::from))
                            .unwrap_or_else(|| {
                                Response::builder()
                                    .status(StatusCode::SERVICE_UNAVAILABLE)
                                    .body(Default::default())
                                    .unwrap()
                            })
                    })
                    .right_future(),
            })
        }
    })
}
