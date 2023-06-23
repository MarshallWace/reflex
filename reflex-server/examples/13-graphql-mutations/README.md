# Reflex Server Example: GraphQL mutations

This example demonstrates how to deal with GraphQL mutations in a Reflex Server application.

In GraphQL, mutations provide the interface for performing imperative server-side actions. In Reflex, performing an imperative action always requires an idempotency token. This means that in order to perform some action in response to an incoming GraphQL mutation operation, we must first obtain an idempotency token that is unique to that particular mutation operation.

Internally, Reflex Server generates a unique 'request token' for each incoming GraphQL operation, which is an idempotency token that can be used to uniquely identify the operation. The request token is exposed to the graph definition via the 'callback' form of the `Resolver` constructor - i.e. `new Resolver((token) => graph)`. As seen in this example, this mechanism allows the unique request token to be used at any points in the graph definition where it is needed.

While the request token's primary use is for disambiguating individual mutation operations, it is also accessible when defining the query and subscription graph roots. The query and subscription graph responses typically should not contain any data unique to a specific incoming request, however sometimes the request token can be useful for internal logging etc. Be careful though: any expression that depends on the request token will not be cached across multiple requests, and therefore could incur a severe performance hit. For this reason it is recommended that request-level observability requirements for non-mutating operations are implemented as native runtime extensions rather than in the user graph definition.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-server --port 8080 ./index.js
```
> This will serve a GraphQL playground at http://localhost:8080/

## Example query

```
mutation {
  user(id: 1) {
    setName(value: "Reflex") {
      email
      name
    }
  }
}
```

## Example response

```
{
  "data": {
    "user": {
      "setName": {
        "email": "Sincere@april.biz",
        "name": "Reflex"
      }
    }
  }
}
```
