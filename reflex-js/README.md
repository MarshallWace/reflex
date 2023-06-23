# ReflexJS

> JavaScript syntax for Reflex

## What is ReflexJS?

ReflexJS provides a JavaScript-like syntax for the [Reflex](..) reactive language ecosystem.

In practical terms, ReflexJS is a fully reactive implementation of a language with familiar JavaScript syntax.

For example, consider the following JavaScript module:

```javascript
const timestamp = Date.now(); // Get the current system timestamp (ms)

export default `Current UNIX time: ${Math.floor(timestamp / 1000)}`;
```

This module's default export will evaluate to a string such as the following:

```
Current UNIX time: 1695078000
```

The actual value will be determined by the system date at the point of evaluation, and will be fixed for the duration of the program: the expression's value never changes once evaluated.

Consider the equivalent ReflexJS module:

```javascript
import { now } from 'reflex::time';

const timestamp = now({ interval: 1000 }); // Re-emits every 1000ms

export default `Current UNIX time: ${Math.floor(timestamp / 1000)}`;
```

When evaluated in Reflex, this module will evaluate to a sequence of strings such as the following:

```
Current UNIX time: 1695078000
Current UNIX time: 1695078001
Current UNIX time: 1695078002
...[re-emits a new result every 1000ms]
```

This expresses the notion of *reactivity*: every time a new timestamp value is emitted, any expression that depends on the timestamp will be automatically recomputed and a new result will be emitted. In other words, every expression automatically reacts to changes in its underlying dependencies.

## Differences between ReflexJS and ECMAScript

ReflexJS is not intended to be a spec-compliant implementation of ECMAScript, instead supporting a strict subset of ECMAScript syntax that is consistent with a pure-functional runtime environment.

### Syntactic differences between ReflexJS and ECMAScript

ReflexJS syntax is a strict subset of ECMAScript syntax, which means that all ReflexJS syntax is therefore valid ECMAScript syntax.

The opposite does not hold though: many valid ECMAScript syntax elements are not currently supported in ReflexJS.

Any unsupported ECMAScript syntax will be rejected ahead-of-time when parsing ReflexJS, so there is no chance of accidentally encountering an  error at runtime due to unrecognized ReflexJS syntax.

#### Unsupported language features

Various ECMAScript language features are intentionally unsupported as a result of deliberate design decisions:

- Mutation (unintuitive in a reactive environment)
  - Reassignable `let` / `var` variables
  - Impure functions
  - Dynamic property assignment
  - Dynamic prototype-based inheritance
- Async APIs (unnecessary/implicit in a reactive environment)
  - `async`/`await`/`Promise`
  - Asynchronous generators
- Dynamic evaluation (not implemented due to security/sandboxing concerns)
  - `eval()`
  - `new Function()`

Additionally, the ReflexJS standard library omits all functionality that relies on side-effects, such as the following:

  - `Math.random()`
  - `Date.now()`
  - `setTimeout()`
  - `fetch()`

> Equivalent functionality is typically provided in Reflex via library helpers.

ReflexJS is intended as a cross-platform language, so browser-only APIs are not implemented for compatibility reasons.

Various ECMAScript language features are not yet supported, however may be supported in future:

- Named [function declarations](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function) / [function expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function) (use [arrow syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions) instead)
- Class declaration syntax
- Various logical / numeric operators
- Various `Math` methods
- Various `Array.prototype` / `Map.prototype` / `Set.prototype` methods
- Web Workers

### Semantic differences between ReflexJS and ECMAScript

For a given program that is syntactically valid in both ReflexJS and ECMAScript, ReflexJS has the following major semantic divergences from ECMAScript:

#### Identity / equality

In ReflexJS, if the values on both sides of an equality expression are structurally equivalent, the values are identical (and therefore equal):

```shell
$ reflex-cli --syntax js
> 3 === 3
true
> [3] === [3]
true
> ({ foo: [3] }) === ({ foo: [3] })
true
> ((x, y) => x + y) === ((y, z) => y + z)
true
> ((x, y) => x + y) === ((y, z) => z + y)
false
```


This differs from ECMAScript, which has the notion of [object identity](https://tc39.es/ecma262/#sec-identity) (this concept does not exist in ReflexJS).

#### `==` vs `===`

In ReflexJS, the concepts of strict equality and loose equality do not exist, there is only structural equality.

The `==` and `===` operators are interchangeable, and express the notion that two values are structurally equal.

#### `null` vs `undefined`

In Reflex, `null` and `undefined` are identical and therefore interchangeable.

#### Accessing undefined object properties

Unlike in ECMAScript, accessing non-existent object properties will throw an exception in ReflexJS.

To retrieve the value of an object property which may or may not exist, you must first check for the existence of the property using the `in` operator:

```javascript
const foo = { bar: true };
const value = 'bar' in foo ? foo.bar : false;
```

#### Numeric types

In ECMAScript, the `Number` type represents a floating point value, and is typically used to represent both floating point values and integer values.

Reflex however differentiates internally between integer values and floating point values.

This is usually not a problem when writing ReflexJS modules due to the following constraints:

- Numeric literals in ReflexJS source code (e.g. the token `3`) are parsed as floating point values, not integers

- Reflex standard library functions that expect integer arguments will typically also accept whole-number floating point values

Problems can arise however when ReflexJS modules interact with data that has been loaded using different conversion rules.

An example of this is when working with JSON payloads that contain records with integer fields. Consider the following ReflexJS:

```javascript
// Parse a JSON string containing records with integer IDs
const items = JSON.parse('[{"id":1,"value":"foo"},{"id":2,"value":"bar"}]');

// Construct a map of records, keyed by record ID
const itemsById = new Map(items.map((item) => [item.id, item]));

// Attempt to retrieve individual records from the map
const works = itemsById.get(items[1].id);
const alsoWorks = itemsById.get(parseInt(2));
const doesntWork = itemsById.get(2);
```

In this example, the `works` and `alsoWorks` records will be retrieved successfully, due to being retrieved with an integer key. The `doesntWork` record will not be found, due to attempting to retrieve the record with a floating point key (remember that the `2` numeric literal token in ReflexJS source code will be parsed as a floating point value).

This is because under Reflex's JSON parsing rules, JSON numeric literals that do not contain a decimal point separator character will be parsed as integers. This means that the `id` field will be parsed as an integer value, and therefore the record must be retrieved from the `Map` by the integer key rather than the float equivalent due to the strict equality comparison that the `Map` implementation uses to match its entries.

As seen in the `alsoWorks` example, the solution to this problem is to convert values between integer and float types using the `parseInt()` and `parseFloat()` standard library functions.

The following comparisons can be useful to bear in mind when considering numeric types in ReflexJS:

```shell
$ reflex-cli --syntax js
> 3 === parseFloat(3)
true
> 3 === parseInt(3)
false
> parseFloat(3) === parseInt(3)
false
> JSON.parse('3') === 3
false
> JSON.parse('3') === parseInt(3)
true
> JSON.parse('3.0') === 3
true
> JSON.parse('3.0') === parseInt(3)
false
```

#### Function call argument evaluation order

In ECMAScript, function call arguments are strictly evaluated in sequence from left to right. If an exception is encountered when evaluating any argument, no further arguments are evaluated and the exception bubbles up the call stack.

In ReflexJS, the pure-functional nature of the language means that function call arguments can be evaluated in any order (potentially in parallel). This means that all arguments will be evaluated regardless of whether the evaluation of any of the other arguments encounters an exception. Depending on the number of exceptions encountered when processing all the arguments, this leads to one of the following scenarios:

- *Scenario 1: No exceptions encountered when evaluating function arguments*
  - Function is invoked with the evaluated argument values
- *Scenario 2: One exception encountered when evaluating function arguments*
    - Function is not invoked
    - Encountered exception bubbles to call site
- *Scenario 3: Multiple exceptions encountered when evaluating function arguments*
    - Function is not invoked
    - All exceptions encountered when evaluating arguments are aggregated into a single combined [`AggregateError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AggregateError) object whose list of [`errors`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AggregateError/errors) contains the exceptions encountered when evaluating arguments in left-to-right order (any nested `AggregateError` objects are flattened into a single aggregated error object)
    - Aggregated exception bubbles to call site

## License

This software is distributed under the Apache 2.0 license. See the full [`LICENSE`](./LICENSE) text for details.
