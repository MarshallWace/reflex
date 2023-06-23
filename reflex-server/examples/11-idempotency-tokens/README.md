# Reflex Server Example: Idempotency tokens

This example demonstrates how to invalidate stateful data within Reflex.

All programs written in ReflexJS are guaranteed to be pure-functional: any expression that relies on data from the outside world must achieve this declaratively via a system of 'effects' and 'effect handlers'.

This means that unlike in traditional procedural programming languages, in Reflex you cannot imperatively force a side-effect to occur at a given moment. Instead, an expression can 'depend on' the value corresponding to an 'effect', where an effect is a declarative description of a side-effect that ultimately resolves to a real-world value.

When an effect is encountered in a user program, the Reflex runtime is responsible for parsing the effect details and invoking the relevant effect handler to obtain the actual value. This value is then provided to the program so that the expression can be re-evaluated with the resolved effect value.

Reflex effect handlers are assumed to be *idempotent*: repeated attempts to handle a given effect are expected to return the original result and not cause additional side-effects.

Importantly, two identical effects will be treated as interchangeable regardless of where in the program they occur. This means that in order to invalidate or reload data, we must change some aspect of the effect so that the runtime treats the two instances as different effects.

This is achieved by specifying an *idempotency token*, which can be provided as an argument to the various effect handler APIs. An idempotency token is a symbol that determines uniqueness between otherwise-identical effects, and is typically derived from one of the following:

- a value or callback argument exposed by an imported library helper
- invoking the `hash(...)` helper function on a set of uniqueness-determining values
- a combination of both of the above

It might sometimes appear tempting to indicate an 'immutable' effect that can never be invalidated, by providing a static token whose value does not itself rely on any stateful dependencies (and therefore will never change throughout the lifetime of the program). This is usually an anti-pattern: an effect which can never be invalidated indicates that the underlying data can never change, and therefore could just as well be compiled into the source code rather than loaded in at runtime via an effect handler. For most real-world applications, it is more likely that even a globally-shared 'singleton' effect ought to be invalidated periodically or retried on encountering an error, leading to more robust fault-tolerant applications.

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
