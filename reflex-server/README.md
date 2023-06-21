# Reflex Server

> Subscription-based GraphQL Reflex server implementation

## What is Reflex Server?

Reflex Server is an out-of-the-box HTTP/WebSocket server implementation that exposes the [Reflex](..) reactive programming language ecosystem via a GraphQL API.

## Getting started

Build the `reflex-server` executable via `cargo`:

```shell
cargo build --release --workspace --package reflex-server --bin reflex-server
```

Once the `reflex-server` executable has compiled successfully, launch it with a graph definition entry-point script to expose an HTTP/WebSocket GraphQL server on the specified port:

```shell
$ reflex-server --port 8080 \
    --schema ./reflex-server/examples/timestamp/schema.graphql \
    ./reflex-server/examples/timestamp/graph.js
```

You can then access the GraphQL playground at http://localhost:8080/. The GraphQL playground will provide type completion based on the provided `--schema` path.

Try running a query such as the following:

```graphql
query {
  now
}
```

Streaming subscriptions are also supported:

```graphql
subscription {
  now
}
```

For some examples of graph definitions, see the [`examples`](./examples) directory within this package.

For more information on the various `reflex-server` CLI options, run the `reflex-server --help` command.

## License

This software is distributed under the Apache 2.0 license. See the full [`LICENSE`](./LICENSE) text for details.
