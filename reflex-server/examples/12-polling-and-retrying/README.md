# Reflex CLI Example: Polling and retrying

This example demonstrates how to use simplified library helpers to periodically poll for updated values and retry errors within Reflex.

The `poll(duration, factory)` helper can be used to transform a one-off response into a value that is automatically re-fetched at a given time interval. This is particularly useful when interacting with servers that do not support streaming responses.

The helper invokes the `factory` function initially and then re-invokes it on an interval of every `duration` milliseconds.

The `poll` function takes 2 arguments:

- `duration`: milliseconds after which to re-invoke the `factory` function
- `factory`: function that returns the result, and takes 1 argument:
    - `token`: Invalidation token for the current poll

The return value will be the return value of the latest invocation of the `factory` function.

The `retryErrors(options, factory)` helper can be used to automatically re-fetch a value whenever an error result is encountered. This is particularly useful when implementing 'self-healing' routines for long-running subscription-based services.

The helper invokes the `factory` function initially and then re-invokes it whenever an exception is encountered.

The `retryErrors` function takes 2 arguments:

- `options`: Object containing the following fields:
    - `delay`: Function that returns the number of milliseconds to wait until the `factory` function is re-invoked. The `delay` function takes 1 argument:
        - `retryIndex`: Zero-indexed integer that indicates the number of failed attempts since the last successful result. The `retryIndex` will increase over a run of consecutive error results, and will reset to zero whenever a successful result is encountered
    - `timeout`: Timeout in milliseconds before abandoning the current attempt with a `TimeoutError` and retrying after the given `delay`
    > Note that the `delay` must be a pure function that is a stateless transformation of its arguments
- `factory`: function that returns the result, and takes 1 argument:
    - `token`: Invalidation token for the current retry attempt

The return value will be the return value of the latest invocation of the `factory` function.

The `backoff` library helper is used to ensure an increasing delay between consecutive error results, up to a predetermined limit. This is provided as a sensible default to use for the `retryErrors` delay function.

As seen in this example, it is common to combine multiple invalidation helpers, each with their own idempotency tokens. Note how tokens can be combined using the `hash(...)` library helper, which ensures that whenever one of the tokens changes, the overall combined token will change.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-server --port 8080 ./index.js
```
> This will serve a GraphQL playground at http://localhost:8080/

## Example query

```
subscription {
  now
}
```

## Example response

```
{
  "data": {
    "now": "Current UNIX time: 1695078000"
  }
}
{
  "data": {
    "now": "Current UNIX time: 1695078001"
  }
}
{
  "data": {
    "now": "Current UNIX time: 1695078002"
  }
}
```
