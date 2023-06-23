# Reflex Server Example: Pending placeholders

This example demonstrates how to show placeholders for pending values within Reflex.

Note that without the `ifPending()` helper, the asynchronous data load would prevent the overall query from emitting a response until the data has finished loading.

When resolving a streaming server response, rather than block the entire query, often a more user-friendly approach is to provide a placeholder value to fall back to while the data is loading, in order to provide status updates to the user or to return a more immediate partial response while additional non-critical data continues to load in the background.

The first argument to the `ifPending()` function is the potentially-asynchronous expression to evaluate. The second argument is the value that will be used as a substitute while the first argument is being resolved in the background.

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
  user(id: 1) {
    email
    name
  }
}
```

## Example response

```
{
  "data": {
    "user": {
      "email": "Loading...",
      "name": "Loading..."
    }
  }
}
{
  "data": {
    "user": {
      "email": "Sincere@april.biz",
      "name": "Leanne Graham"
    }
  }
}
```
