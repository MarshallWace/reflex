# Reflex CLI Example: External data

This example demonstrates how to load external data within Reflex.

In this example, the data is loaded from the public [JSON Placeholder](https://jsonplaceholder.typicode.com/) dummy API.

Note how in ReflexJS, there is no need to await promises: you can access the `.json()` method as if it were synchronously available, and it will be invoked as soon as the data arrives.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-cli ./index.js
```

## Example output

```
"Hello, Leanne Graham!"
```
