# Reflex Server Example: Field arguments

This example demonstrates how to handle GraphQL field arguments within Reflex Server.

The name to use in the response message is provided in the GraphQL query via the `name` field variable. This is exposed within the graph definition as an `name` field on the field resolver function's arguments object.


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
  hello(name: "Reflex")
}
```

## Example response

```
{
  "data": {
    "hello": "Hello, Reflex!"
  }
}
```
