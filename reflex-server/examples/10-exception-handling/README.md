# Reflex Server Example: Exception handling

This example demonstrates how to handle exeptions within Reflex.

In ReflexJS, exceptions are typically handled using `try...catch` blocks. There is no differentiation between how to handle asynchronous exceptions versus synchronous exceptions.

Any *unhandled* exceptions encountered by Reflex Server will cause a GraphQL error response containing the error details in its `errors` field.

In this example, the initializer of the `message` variable is an [immediately invoked function expression (IIFE)](https://developer.mozilla.org/en-US/docs/Glossary/IIFE). This is because in ReflexJS, every expression must evaluate to a value: in other words, each branch of the `try...catch` block must return a single value. It would not be valid to write a `return` statement at the top level of the module, nor could we write multiple `export default` statements to cover the two branches, so in order to use the `try...catch` block we must wrap it in a function.

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
  message
}
```

## Example response

```
{
  "data": {
    "message": "Fetch error: Invalid HTTP URL: http://@@@"
  }
}
```
