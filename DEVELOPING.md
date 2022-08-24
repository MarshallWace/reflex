# Reflex developer guide

## Codebase structure

The codebase is largely split between two 'worlds', which roughly speaking deal with 'sync' vs 'async' concerns:
  - Core engine (sync):
    - `reflex`: core package that provides primitives for a modular compiler/interpreter framework
    - `reflex-lang`: default set of primitive types that define what terms can be expressed in the language
    - `reflex-stdlib`: default set of builtin core helper functions (arithmetic, control flow, equality operators etc)
    - ...various 'batteries-included' plugins for various technologies used in real-world apps (`reflex-graphql`, `reflex-js`, `reflex-protobuf`, etc)
  - Runtime (async)
    - `reflex-runtime`: asynchronous wrapper around the core engine/interpreter (work scheduling, coordinates loading external data, etc)
    - `reflex-cli`: exposes a `reflex-runtime` instance to the outside world via a REPL
    - `reflex-server`: exposes a `reflex-runtime` instance to the outside world via a GraphQL HTTP/WebSocket server

The sync world generally comprises language-related issues (parsing/compiling/evaluating/etc), whereas the async world generally comprises 'real-world' concerns (scheduling/fetching/transport/etc).

Generally speaking, 'runtime' refers to the async parts of the system while 'core engine' refers to the sync part.

Note that some crates provide both sync and async features - e.g. the `reflex-grpc` package provides sync tools for parsing a protobuf service definition into the language, alongside an async handler that is used at runtime to load data from the service.

There are also a couple of general-purpose utility crates:
  - `reflex-dispatcher`: a simple 'actor model' library (see below)
  - `reflex-utils`: miscellaneous helper functions

## How it works: Core engine

The main job of the core engine is 'evaluating an expression'. This can be done through various means and at various times, but in a nutshell it typically entails the following (pretty traditional) phases:

1. Compiler front-end: parse incoming source code from some real-world input format (JS/GraphQL/etc) into an internal representation AST (loosely based around the lambda calculus primitives)
2. Compiler 'middle-end': optimise that AST into a more efficient AST (by pre-evaluating static expressions into their simplest form)
3. Compiler back-end: compile the optimised AST into a linear list of bytecode instructions (using a fairly simple proprietary instruction set)
4. Interpreter: run the bytecode through a custom stack-based interpreter (whose stack values are the same kind of AST nodes as in the IR); when a well-formed program finishes executing the stack be left with a single item which is the result of evaluating the expression (along with a list of the 'side-effect' dependencies encountered when evaluating that expression, as described below)
5. Garbage collection: after evaluating an expression, any previously-allocated AST nodes that are no longer considered 'active' (i.e. required for the expression evaluation) can be de-allocated. Any AST nodes that are still in use will be retained.

All these phases are currently performed using our own custom implementations (as opposed to using off-the shelf tools such as LLVM / GraalVM etc).

All expressions within the language are pure and immutable - anything that relies on dynamic state happens 'out of band' in the runtime wrapper.

### How it works: Runtime

The primary advantage of reflex is its ability to handle real-time streaming data that is constantly changing, at the language-level (rather than users having to rely on FRP libraries etc). Any time a stateful value changes, that change will trigger re-evaluation of any expressions that depend on that value. This re-evaluation is coordinated by the runtime based on automatic dependency-tracking.

If an expression depends on dynamic run-time state, evaluating that expression will result in a type of value called a 'signal'. This is one of the core language data types, and acts as a marker that indicates to the runtime that the expression depends on run-time state which must be loaded (and dependency-tracked) in order to evaluate the expression.

- a signal is essentially a list one or more 'effects' (another core language data type)
- effects are simple data objects that describe a task that must be fulfilled with a concrete value before evaluation can proceed (e.g. 'fetch this URL')
- this means that signals block evaluation: when an expression evaluates to a signal, it's the responsibility of the runtime to go off and do whatever real-world (potentially-async) side-effects necessary to 'handle' that effect and yield the correponding value
- you can think of a signal as an 'await' point (or a bit like a thrown exception, or maybe more accurately as a Scheme 'continuation', or a Common Lisp 'condition'), with the difference that signals don't halt evaluation of parallel code paths
- for example, when applying a function that takes multiple arguments, if evaluating the first argument results in a signal, the function body will not be invoked but the other arguments will still be evaluated. In this case the overall result of evaluating the function application expression will be a single aggregated signal that contains the union of all the effects from all the signals encountered while evaluating the arguments
- in practice this means that any signals bubble up to the top level, combining with other signals along the way, to form a single overall signal that can contain many effects encountered from the various different execution paths (rather than each signal individually blocking execution like an exception/await would behave in most languages)
- this means that a typical real-world application is able to process whole sets of side-effects simultaneously as a batch, with the multiple-code-path execution greedily discovering any stateful dependencies, therefore ensuring the minimal number of pauses for blocking asynchronous events
- the real-world action performed in response to an effect is determined by which 'effect handler' plugins where loaded when instantiating the runtime
- given that an effect is just a declarative description of an action rather than anything concerning the implementation, effect handlers can trivially be swapped out / decorated / mocked etc with totally different implementations if desired
- once the effect handler has (asynchronously) yielded a value, this triggers the runtime to reattempt evaluating the expression, this time passing in a 'state of the world' key/value object argument that contains the latest value yielded for this effect (plus the values for any other previously-encountered effects)
- this time around, whatever blocking operation returned the signal will now be able to proceed seeing as it now has a value for that effect, provided by the state object (all such 'satisfied' effects encountered along the way will be added to the list of dependencies encountered during evaluation)
- this process continues until there are no more blocking signals, at which point evaluation can complete successfully
- effect handlers can emit multiple values over time to reflect streaming data; subsequent values yielded by the handler will retrigger evaluation of anything that has a dependency on that effect
- effect results are automatically deduplicated and shared between any identical effects (this can be cache-busted using invalidation 'tokens')
- the pure functional approach means that this evaluation model ought to be very amenable to being parallelised (the tricky part is in determining which code paths are worth forking into a parallel thread)
