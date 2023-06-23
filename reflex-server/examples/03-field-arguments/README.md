# Reflex Server Example: Field arguments

This example demonstrates how to handle GraphQL field arguments within Reflex Server.

In additional to static fields, all fields within Reflex graph definitions can be parameterized by field arguments.

As seen in this example, Reflex Server allows defining a parameterized field as one whose value is a function that takes an 'arguments' object containing the named field argument values provided by the query. In this case, the field resolver expects a single argument named `name`.

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
