# Reflex CLI Example: Reactivity

This example demonstrates how reactive updates are automatically propagated within Reflex.

In this example, the timestamp value re-emits every 1000ms, and the output label is automatically recomputed every time a new timestamp is emitted.

Note that the `timestamp` variable is declared using a standard `const` declaration, but its value will automatically track the live result of the `now()` call.

Also note that updates are propagated via push-based 'signals', not via pull-based polling. In other words, as soon as a new value is emitted from the timestamp signal, a new overall result will be immediately recalculated.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-cli ./index.js
```

## Example output

```
"Current UNIX time: 1695078000"
"Current UNIX time: 1695078001"
"Current UNIX time: 1695078002"
```
