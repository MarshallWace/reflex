# Reflex Server Example: Accumulating values

This example demonstrates how to work with stateful values that are accumulated over time within Reflex.

When working with real-time event sources, it is often useful to track the current state of a value that accumulates over time in response to incoming events.

As seen in this example, this can be achieved in Reflex by using the `scan` library helper. This helper models a value that accumulates over time, based on watching an input expression for changes and applying a deterministic 'reducer' function on every change to the input expression.

The result of a `scan(source, seed, reducer)` expression represents the current state of the accumulated value. The arguments to the `scan(...)` function are as follows:

- `source`: the input expression to watch for updates. Every time the input expression's value changes, the `reducer` function will be re-invoked with the updated value.
- `seed`: seed value to pass as the `reducer` function's `state` argument for the initial iteration
- `reducer`: function that returns the overall accumulated result, and takes 2 arguments:
    - `state`: latest accumulated result, or the `seed` value if this is the first invocation of the reducer
    - `value`: latest value of the `source` expression
> Note that the `reducer` must be a pure function that is a stateless transformation of its arguments

While this is a fairly advanced helper, it can be used to model arbitrarily complex state machines and is therefore a very versatile tool in the right hands.

The primary danger when using the `scan` helper is that it operates over time, exposing the results from a previous evaluation pass to the currrent evaluation pass. This makes it possible to accidentally accumulate an increasingly large `state` object, which grows indefinitely until the program consumes all available memory. Care must therefore be taken to ensure that the size of the state object remains roughly constant over the lifetime of the program in order to avoid memory leaks.

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
  buckets
}
```

## Example response

```
{
  "data": {
    "buckets": [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  }
}
{
  "data": {
    "buckets": [1, 1, 0, 0, 0, 0, 0, 0, 0, 0]
  }
}
{
  "data": {
    "buckets": [2, 1, 0, 0, 0, 0, 0, 0, 0, 0]
  }
}
```
