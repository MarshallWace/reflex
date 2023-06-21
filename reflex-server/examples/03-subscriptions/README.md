# Reflex Server Example: Reactivity

This example demonstrates how reactive updates are automatically propagated within Reflex.

In this example, the timestamp value re-emits every 1000ms, and a new GraphQL subscription result is automatically pushed to the client via the Web Socket connection every time a new timestamp is emitted.

Note that updates are propagated via push-based 'signals', not via pull-based polling. In other words, as soon as a new value is emitted from the timestamp signal, a new overall result will be immediately recalculated.

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
    "now": "Current UNIX time: 1695079000"
  }
}
{
  "data": {
    "now": "Current UNIX time: 1695080000"
  }
}
```
