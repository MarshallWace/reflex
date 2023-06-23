# Reflex CLI Example: Environment variables

This example demonstrates how to access environment variables within Reflex.

Similarly to Node.js, the global `process.env` object contains the set of environment variables that the Reflex runtime executable was invoked with.

Note how in ReflexJS, any attempt to access an undefined object field will raise an exception, hence the need for the `'...' in process.env` check to handle the case where the environment variable has not been declared.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ APP_USER=Reflex reflex-server --port 8080 ./index.js
```
> This will serve a GraphQL playground at http://localhost:8080/

## Example query

```
query {
  hello
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
