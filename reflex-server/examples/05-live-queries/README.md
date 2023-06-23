# Reflex Server Example: Live queries

This example demonstrates how the results of arbitrarily complex reactive queries are automatically propagated within Reflex.

GraphQL subscriptions have traditionally been used to model simple 'event sources', which emit atomic messages whenever the underlying source emits an event. This is reinforced by the GraphQL specification, which specifies that [subscription roots must only have a single source field](http://spec.graphql.org/October2021/#sec-Single-root-field). This restriction is not enforced by Reflex Server, which has been designed to support fully-fledged 'live queries' that go far beyond traditional atomic event sources.

All expressions in Reflex (whether primitive values, object fields, array items, etc) are fully reactive and will automatically propagate any changes to the overall top-level query. This means that without any additional bookkeeping work, Reflex Server allows an arbitrarily complex query to be fetched as a subscription, and the response will automatically be kept up-to-date.

As seen in this example, a common pattern in Reflex Server is to use the same graph definition root for both `query` and `subscription` operation roots. This means that the same query shape can be retrieved either as a one-off `query` operation, or as a live `subscription` operation, according to the needs of the consumer.

Something to keep in mind with live queries (as opposed to atomic subscriptions) is that by default, the whole response payload will be sent whenever any field within the response changes. This can become problematic for large queries that experience frequent small updates. Reflex Server solves this problem by introducing a 'diff mode', that sends down the initial full payload followed by a sequence of minimal 'patch' updates that represent changes to the overall payload.

To enable diff mode for a given query, Reflex Server supports a proprietary extension to the [graphql-transport-ws](https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md) protocol: when sending the [`GQL_START`](https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_start) message, clients may append an additional `"extensions": { "diff": true }` field to the `payload` object. This will cause the server to send the initial [`GQL_DATA`](https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data) response as usual, but this will then followed by a series of proprietary `"type": "patch"` messages, whose `"payload"` field is a sparse object containing just those fields that have changed. Arrays whose lengths have changed as a result of having items added or removed will include an additional `"length"` field that reflects the updated length. These patch messages can be spliced onto the existing result on the client side to obtain an up-to-date response for the overall live query.

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
  sampled(interval: 100) {
    labeled(prefix: "High-resolution timestamp") {
      seconds
    }
  }
}
```

## Example response

```
{
  "data": {
    "now": "Current UNIX time: 1695078000",
    "sampled": {
      "labeled": {
        "seconds": "High-resolution timestamp: 1695078000.000"
      }
    }
  }
}
{
  "data": {
    "now": "Current UNIX time: 1695078000",
    "sampled": {
      "labeled": {
        "seconds": "High-resolution timestamp: 1695078000.100"
      }
    }
  }
}
{
  "data": {
    "now": "Current UNIX time: 1695078000",
    "sampled": {
      "labeled": {
        "seconds": "High-resolution timestamp: 1695078000.200"
      }
    }
  }
}
```
