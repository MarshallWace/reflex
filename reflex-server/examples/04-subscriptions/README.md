# Reflex Server Example: Subscriptions

This example demonstrates how reactive updates are automatically pushed to subscribed clients within Reflex Server.

In this example, the timestamp value re-emits every 1000ms, and a new GraphQL subscription result is automatically pushed to the client via the Web Socket connection every time a new timestamp is emitted.

Note that the `timestamp` variable is declared using a standard `const` declaration, but its value will automatically track the live result of the `now()` call.

Also note that updates are propagated via push-based 'signals', not via pull-based polling. In other words, as soon as a new value is emitted from the timestamp signal source, a new overall result will be immediately recalculated and sent to the client (assuming the response payload has changed). If the response payload is identical to the previous value, no update will be sent to the client.

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
