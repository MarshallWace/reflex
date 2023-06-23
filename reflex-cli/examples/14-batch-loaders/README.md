# Reflex CLI Example: Batch loaders

This example demonstrates how to batch multiple data loads into a single request within Reflex.

In this example, the data is loaded from the public [JSON Placeholder](https://jsonplaceholder.typicode.com/) dummy API, which allows you to retrieve multiple entities in a single request.

In Reflex, the `DataLoader` API allows you to create a new batch loader by providing a factory function that is used to load a set of multiple entities as a single combined batch.

The factory function accepts an array of entity IDs as an argument, and must return the corresponding values in one of the following forms:

- a `Map` instance containing an entry for each of the requested IDs
- an `Array` instance whose values correspond 1:1 with the list of requested IDs (in the same order)

Given a batch loader instance, any `.load(id)` or `.loadMany(ids)` calls to that loader will cause the loader's factory function to be invoked with the full set of IDs requested in the current batch.

While this has the appearance of an imperative API, the effect-loading mechanism which underpins it is actually fully declarative and therefore allows multiple requests for entities to be grouped together into a single combined set.

As seen in this example, all entity loads within the same evaluation pass will therefore be batched together, resulting in an optimal number of loads and avoiding the "N + 1 query problem".

> Note that contrary to what you might expect from a traditional programming language, this batching happens synchronously and does not introduce any asynchronous delays or debouncing. This is possible because Reflex's declarative effects system allows multiple independent effect-based expressions to be evaluated concurrently without blocking each other, so all the effects encountered within the same evaluation pass can be gathered up together and processed simultaneously by the effect handler.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-cli ./index.js
```

## Example output

```
"Hello, Leanne Graham and Clementine Bauch!"
```
