# Reflex CLI Example: Pending placeholders

This example demonstrates how to show placeholders for pending values within Reflex.

Note that without the `ifPending()` helper, the asynchronous data load would prevent the overall program from emitting a result until the data has finished loading.

Rather than block the entire program, often a more user-friendly approach is to provide a placeholder value to fall back to while the data is loading, in order to provide status updates to the user or to return a more immediate partial response while additional non-critical data continues to load in the background.

The first argument to the `ifPending()` function is the potentially-asynchronous expression to evaluate. The second argument is the value that will be used as a substitute while the first argument is being resolved in the background.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-cli ./index.js
```

## Example output

```
"Loading..."
"Hello, Leanne Graham!"
```
