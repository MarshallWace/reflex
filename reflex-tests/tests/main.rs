// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use serde_json::{json, Value};

use reflex_graphql::subscriptions::GraphQlSubscriptionServerMessage;

use crate::client::GraphQlConnection;

mod client;
mod server;

#[tokio::test]
async fn duplicate_subscriptions() {
    let input = r#"
    import { Resolver } from 'reflex::graphql';
    
    export default new Resolver({
      query: null,
      mutation: null,
      subscription: {
        foo: 'hi',
      },
    });"#;

    let (server_addr, server_killswitch) = server::serve_graphql(input);

    let mut client1 = GraphQlConnection::new(&server_addr).await;
    let client1value1 = client1
        .send_client_message_and_receive(GraphQlConnection::start_query_message(
            1,
            "subscription {\n  foo\n}\n",
            None,
        ))
        .await;
    assert_eq!(
        &client1value1,
        &GraphQlConnection::data_message(1, json!({"data":{"foo": "hi"}}))
    );

    let mut client2 = GraphQlConnection::new(&server_addr).await;
    let client2value1 = client2
        .send_client_message_and_receive(GraphQlConnection::start_query_message(
            2,
            "subscription {\n  foo\n}\n",
            None,
        ))
        .await;

    assert_eq!(
        &client2value1,
        &GraphQlConnection::data_message(2, json!({"data":{"foo": "hi"}}))
    );
    assert_eq!(
        get_typed_payload(client2value1),
        get_typed_payload(client1value1)
    );

    server_killswitch.send(()).unwrap();
}

#[tokio::test]
async fn modified_subscriptions() {
    let input = r#"
    import { Resolver } from 'reflex::graphql';
    
    export default new Resolver({
      query: null,
      mutation: null,
      subscription: {
        foo: ({ id }) => `ID: ${id}`,
      },
    });"#;

    let (server_addr, server_killswitch) = server::serve_graphql(input);

    let mut client1 = GraphQlConnection::new(&server_addr).await;
    let client1value1 = client1
        .send_client_message_and_receive(GraphQlConnection::start_query_message(
            1,
            "subscription {\n  foo(id: \"foo\")\n}\n",
            None,
        ))
        .await;
    assert_eq!(
        &client1value1,
        &GraphQlConnection::data_message(1, json!({"data":{"foo": "ID: foo"}}))
    );

    let mut client2 = GraphQlConnection::new(&server_addr).await;
    let client2value1 = client2
        .send_client_message_and_receive(GraphQlConnection::start_query_message(
            2,
            "subscription($id: String) {\n  foo(id: $id)\n}\n",
            Some(json!({"id": "bar"})),
        ))
        .await;
    assert_eq!(
        &client2value1,
        &GraphQlConnection::data_message(2, json!({"data":{"foo": "ID: bar"}}))
    );

    let client2value2 = client2
        .send_client_message_and_receive(GraphQlConnection::update_query_message(
            2,
            json!({"id": "foo"}),
        ))
        .await;
    assert_eq!(
        get_typed_payload(client1value1),
        get_typed_payload(client2value2)
    );

    server_killswitch.send(()).unwrap();
}

fn get_typed_payload(response: GraphQlSubscriptionServerMessage) -> Value {
    match response {
        GraphQlSubscriptionServerMessage::Data(_, data) => data.get("data").unwrap().clone(),
        _ => panic!("Expected data"),
    }
}
