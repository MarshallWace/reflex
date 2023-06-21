# Reflex developer guide

## Loading data via effect handlers

Reflex provides various data-loading mechanisms for interfacing with [HTTP](./reflex-http), [gRPC](./reflex-grpc) and [GraphQL](./reflex-graphql) data sources, as well as a [batch-loading helper](./reflex-batch) and a set of helpers for dealing with [stateful computations](./reflex-state).

Stateful computations and data-loading are achieved via various add-on packages (these are already included with the bundled [server implementation](./reflex-server)):

- [`reflex-http`](./reflex-handlers): HTTP fetch support
- [`reflex-grpc`](./reflex-grpc): Client library for interfacing with gRPC servers
- [`reflex-graphql`](./reflex-graphql): Client library for interfacing with GraphQL servers
- [`reflex-batch`](./reflex-batch): Batch data loader (compatible with all other effect handlers)
- [`reflex-state`](./reflex-handlers): Stateful computation helpers

## Extending out-of-the-box Reflex configurations

For the more adventurous, custom data sources can be implemented natively in Rust by implementing the [effect-handler](./reflex-runtime/docs/effect-handler.md) interface, along with an extensible [standard library](./reflex-stdlib) and a set of [base primitives](./reflex) that allow you to implement your own reactive programming languages.

The bundled set of data loaders can be extended by implementing custom support for arbitrary data sources:

- [`reflex-runtime`](./reflex-runtime): Base primitives used to implement custom side-effect handlers

The Reflex standard library is fully extensible, with a common base set of methods that can be enhanced by the specific language implementations:

- [`reflex-stdlib`](./reflex-stdlib): Common standard library methods useful for general-purpose languages

If you want to use Reflex to develop your own languages, you may find the following packages useful:

- [`reflex`](./reflex): Core primitives used to implement custom languages
- [`reflex-lisp`](./reflex-lisp): Simple proof-of-concept that demonstrates how to implement a basic S-expression language with a default set of standard library methods
- [`reflex-js`](./reflex-js): Advanced implementation of a general-purpose JavaScript subset, including various additional standard library methods

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

Signals are handled as follows:

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

Some notes on the runtime implementation:

- the runtime/server application is implemented as a composable state machine (an 'actor'), whose transitions are coordinated by a central 'scheduler'
- everything that happens in the application is triggered via state machine 'transitions', which are dispatched through the scheduler's event bus
- a transition typically contains a list of 'actions', which are serializable data messages, or alternatively a 'task', which is an asynchronous stream of actions over time
- the overall app actor is composed from a collection of smaller actors that handle separate areas of concern (transport, effect-handling, etc). All actors share the same event bus.
- actors have a single synchronous `handle(state, action) -> transition` method that allows them to change their private state in response to an incoming action and return a state transition containing any spawned actions that happen as a consequence of the received action
- outside of their `handle()` methods, actors are immutable (so in other words, nothing can happen outside a state transition)
- this means that actors must communicate entirely via message-passing
- all the composed actors are currently bolted together in the same process, but the shared-nothing design means they could alternatively be arbitrarily distributed across multiple threads or machines
- the standard scheduler is asynchronous (with a tokio mpsc channel for the main event bus), but this can easily be swapped out for an alternative scheduler implementation (e.g. a synchronous in-memory-queue-based scheduler for unit tests)
