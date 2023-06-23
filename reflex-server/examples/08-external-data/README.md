# Reflex Server Example: External data

This example demonstrates how to load external data within Reflex.

In this example, the data is loaded from the public [JSON Placeholder](https://jsonplaceholder.typicode.com/) dummy API.

Note how in ReflexJS, there is no need to await promises: you can access the `.json()` method as if it were synchronously available, and it will be invoked as soon as the data arrives.

Also note how all the fields within the loaded JSON object are automatically exposed to the graph without having to explicitly extract them from the object. In Reflex Server, any objects that are embedded directly into a graph definition will be automatically traversible via the GraphQL query.

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
      "email": "Sincere@april.biz",
      "name": "Leanne Graham"
    }
  }
}
```
