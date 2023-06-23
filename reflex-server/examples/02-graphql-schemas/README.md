# Reflex Server Example: GraphQL schemas

This example demonstrates how to configure Reflex Server to use a GraphQL schema.

Unlike typical GraphQL server implementations, Reflex Server can operate in schemaless mode, where all incoming queries are dynamically resolved by starting at the operation root node and traversing recursively through the query branch selections.

While this can be a timesaver for quick experiments and trivial examples, a better approach for any real applications is to provide a [GraphQL schema](https://graphql.org/learn/schema/) that describes the shape of the graph API.

To configure Reflex Server to use a GraphQL schema, pass the path to a GraphQL SDL schema definition file as the `--schema` command-line argument. This will ensure that all incoming queries are validated against the specified schema before attempting to retrieve their results.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-server --schema ./schema.graphql --port 8080 ./index.js
```

> This will serve a GraphQL playground at http://localhost:8080/

## Example query

```
query {
  hello
  goodbye
}
```

## Example response

HTTP status: `400`

```
"GraphQL query transformation failed: Field \"goodbye\" not found on type Query"
```
