# Reflex architecture overview

## Compiler/interpreter pipeline steps:

### Ahead-of-time:

1. Parse graph root JS (Source file > JS AST)
2. Convert parsed graph root JS to reflex AST types as described below (JS AST > reflex AST)
3. Perform graph normalisation on reflex AST - i.e. evaluate as much of the graph as you can until it's in its simplest form (AST > simpler AST)
4. Graph AST is compiled into list of bytecode instructions (AST > bytecode)

### Query-time:

1. User sends a GraphQL query to be evaluated
    - Gateway validates GQL query against GQL schema, potentially returns 400 Bad Request _(not yet implemented)_
2. GraphQL query is parsed into a GraphQL AST (String > GraphQL AST)
3. Convert parsed query AST to reflex function expression AST (GraphQL AST > Reflex AST)
    - Query root node will be a LambdaTerm that takes 1 argument (the graph root), and whose body is a deeply-nested generated expression which takes the current graph root as an argument and then traverses through the levels to evaluate all the nested leaves, combining everything into a json-compatible response payload
4. Query AST is compiled into a list of bytecode instructions (AST > bytecode)
5. Query bytecode is appended to ahead-of-time-compiled graph bytecode (bytecode > bytecode)
6. Overall expression to evaluate is a function application bytecode instruction that applies the query function to the graph root, i.e. `query(root)` - this is the bytecode interpreter entry point (bytecode > bytecode)
7. Overall bytecode is executed via bytecode interpreter to compute the final output value (bytecode > AST)
8. One of the following actions takes place depending on what the interpreter returned:
  - If the resulting value is a static value, it is JSON-serialized and returned to the client
  - If the resulting value is a 'signal' indicating side-effects, the runtime triggers these side-effects, updates the state as appropriate and re-runs the bytecode against the updated state
  - Any other non-serializable value indicates a programmer error; error response is returned to the client

## Reflex AST node types

Defined in [`reflex/src/lang/term.rs`](`./reflex/src/lang/term.rs`)

```rust
pub enum Term {
  Value, // Static value
  Variable, // Static scoped 'local' variable or dynamic global 'state' variable
  Let, // Define a local variable to be referenced in the inner block
  Lambda, // Function definition
  Application, // Thunk representing a function call
  PartialApplication, // Function + some pre-applied arguments
  Recursive, // Recursive value factory (Y-combinator)
  Builtin, // Built-in function
  Native, // User-provided native function
  CompiledFunction, // Compiled function bytecode instruction address
  Tuple, // Static list of values
  Struct, // Static list of values with accompanying 'prototype' field names
  Constructor, // Constructor function to create a struct from the given prototype
  Collection, // Vector, HashMap, HashSet implementations
  Signal, // Description of one or more side-effects
  SignalTransformer, // Handler to intercept signals
}
```

## `reduce` vs `normalize` vs `evaluate` vs `execute`

> Some background: https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form#answer-6889335

- In addition to static values, a graph can contain various 'thunks' which have not yet been evaluated - primarily function application nodes (i.e. function calls) but also 'let' nodes and variable nodes
- `reduce` takes a graph AST expression and repeatedly reduces the root node until it reaches 'Weak Head Normal Form', where all outer thunks have been evaluated as far as possible (note that this might not be 'all the way'; it can't proceed if it encounters a thunk on the critical path which depends on runtime values not present in the state)
  - Note that `reduce` will only evaluate the 'head' of an expression, not its subexpressions - e.g. list items will not be evaluated, nor will any expressions within a lambda's body
- `normalize` does this plus additionally reduces all the 'unexplored paths', including any lazy function application arguments and subexpressions within lists or function bodies (resulting in an expression in 'normal form')
- `reduce` will perform the minimum possible computations on the critical path so is good for runtime evaluation; `normalize` is much more exhaustive but only feasible at compile-time
- both `reduce` and `normalize` will end up with a partially-evaluated result if they encounter an expression that depends on runtime state we don't yet have
- `evaluate` is used at runtime to compute a result as far as possible by performing a series of reductions interleaved with dynamic variable substitutions as follows:
  1. substitute any dynamic variable terms in the AST with their corresponding runtime state values (if present)
  2. `reduce` the graph root to weak head normal form
  3. if the reduction didn't result in any changes to the AST, we're done
  4. otherwise repeat
- `evaluate` takes an AST node + state, returns fully-evaluated AST node + dependency list
- `execute` is the bytecode equivalent of `evaluate` (takes bytecode program + state; returns fully-evaluated AST value + dependency list)
