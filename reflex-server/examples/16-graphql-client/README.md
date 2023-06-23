# Reflex Server Example: GraphQL client

This example demonstrates how to load external GraphQL data within Reflex.

In this example, the data is loaded from the public [Countries GraphQL API](https://github.com/trevorblades/countries).

To communicate with an external GraphQL API from within ReflexJS, you must import the SDL schema for the external GraphQL API into the ReflexJS module.

The imported schema module exposes as its default export a constructor for a GraphQL client that can be used within ReflexJS to communicate with that API.

As seen in the example, the GraphQL client constructor is instantiated with an object containing a `url` field that denotes the URL of the external GraphQL service to connect to.

Once instantiated, GraphQL queries can be fetched on the resulting API client via the `.execute(options)` method, which returns the parsed GraphQL response for the given query.

The `options` argument must be an object with 4 fields:

- `query`: string containing the GraphQL operation to execute
- `operationName`: Used primarily for observability purposes, this should reference a named GraphQL operation declared in the `query`, or `null` if this is not required
- `variables`: Object containing values for any variables declared in the `query`
- `token`: Idempotency token for this operation

Note how in ReflexJS, there is no need to await asynchronous data fetches: you can access the response data payload as if it were synchronously available, with evaluation proceeding as soon as the data arrives.

While this example demonstrates a GraphQL `query` operation, exactly the same approach is used for `mutation` and `subscription` operations.

When executing a `subscription` operation, Reflex will automatically communicate with the remote server via Web Socket (rather than a standard HTTP request), and the resulting value will change whenever a new result payload is received.

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
  country(code: "GB") {
    capital
    name
  }
}
```

## Example response

```
{
  "data": {
    "country": {
      "capital": "London",
      "name": "United Kingdom"
    }
  }
}
```
