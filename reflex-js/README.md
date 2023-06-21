# ReflexJS

> JavaScript syntax for Reflex

## What is ReflexJS?

ReflexJS provides a JavaScript-like syntax for the [Reflex](..) reactive language ecosystem.

In practical terms, ReflexJS is a fully reactive implementation of a language with familiar JavaScript syntax.

For example, consider the following JavaScript module:

```javascript
const time = Date.now(); // Get the current system timestamp (ms)

export default `Current UNIX time: ${Math.floor(time / 1000)}`;
```

This module's default export will evaluate to a string such as the following:

```
Current UNIX time: 1695078000
```

The actual value will be determined by the system date at the point of evaluation, and will be fixed for the duration of the program: the expression's value never changes once evaluated.

Consider the equivalent ReflexJS module:

```javascript
import { timestamp } from 'reflex::date';

const time = timestamp({ interval: 1000 }); // Re-emits every 1000ms

export default `Current UNIX time: ${Math.floor(time / 1000)}`;
```

When evaluated in Reflex, this module will evaluate to a sequence of strings such as the following:

```
Current UNIX time: 1695078000
Current UNIX time: 1695079000
Current UNIX time: 1695080000
...[re-emits a new result every 1000ms]
```

This expresses the notion of *reactivity*: every time a new timestamp value is emitted, any expression that depends on the timestamp will be automatically recomputed and a new result will be emitted. In other words, every expression automatically reacts to changes in its underlying dependencies.

## Differences between ReflexJS and ECMAScript

ReflexJS is not intended to be a spec-compliant implementation of ECMAScript, instead supporting a strict subset of ECMAScript syntax that is consistent with a pure-functional runtime environment.

### Syntactic differences between ReflexJS and ECMAScript

Any unsupported ECMAScript syntax will be rejected ahead-of-time when parsing ReflexJS, so there is no chance of accidentally encountering an  error at runtime due to unrecognized ReflexJS syntax.

Various ECMAScript language features are intentionally unsupported as a result of deliberate design decisions:

- Mutation (unintuitive in a reactive environment)
  - Reassignable `let` / `var` variables
  - Impure functions
  - Dynamic property assignment
  - Dynamic prototype-based inheritance
- Operations that rely on side-effects (effect-based equivalents provided as helper libraries)
  - `Math.random()`
  - `Date.now()`
  - `setTimeout()`
  - `fetch()`
  - Dynamic `import()`
- Async APIs (unnecessary/implicit in a reactive environment)
  - `async`/`await`
  - `Promise` API
  - Asynchronous generators
- Dynamic evaluation (not implemented due to security/sandboxing concerns)
  - `eval()`
  - `new Function()`
- Browser-only APIs (not implemented for cross-platform compatibility reasons)

Various ECMAScript language features are not yet supported, however may be supported in future:

- Named [function declarations](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function) / [function expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function) (use [arrow syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions) instead)
- Class declaration syntax
- Various logical / numeric operators
- Various `Math` methods
- Various `Array.prototype` / `Map.prototype` / `Set.prototype` methods
- Web Workers

### Semantic differences between ReflexJS and ECMAScript

ReflexJS has the following major semantic divergences from ECMAScript:

#### Identity / equality

In ReflexJS, if the values on both sides of an equality expression are structurally equivalent, the values are identical (and therefore equal):

```
3 === 3 // true
[3] === [3] // true
({ foo: [3] }) === ({ foo: [3] }) // true
((x, y) => x + y) === ((y, z) => y + z) // true
((x, y) => x + y) === ((y, z) => z + y) // false
```

This differs from ECMAScript, which has the notion of [object identity](https://tc39.es/ecma262/#sec-identity) (this concept does not exist in ReflexJS).

This also implies that in ReflexJS, the `==` and `===` operators are interchangeable.

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
