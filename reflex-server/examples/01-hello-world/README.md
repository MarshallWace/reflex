# Reflex Server Example: Hello world

This example demonstrates how to expose a GraphQL API using Reflex Server.

Note that Reflex Server expects the ReflexJS entry point module to expose its result via an `export default` statement whose initializer is a `Resolver` instance.

The simplest way to define a `Resolver` instance is by passing an object to the `Resolver` constructor, where the object contains `query`, `mutation` and `subscription` fields corresponding to the graph roots for the respective GraphQL operation types.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-server --port 8080 ./index.js
```
> This will serve a GraphQL playground at http://localhost:8080/

## Example query

```
query {
  hello
}
```

## Example response

```
{
  "data": {
    "hello": "Hello, world!"
  }
}
```
