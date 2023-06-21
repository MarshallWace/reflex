# Reflex CLI

> Reflex REPL environment and standalone script runner

## What is Reflex CLI?

Reflex CLI is a command-line tool that exposes the [Reflex](..) reactive programming language ecosystem both via an interactive REPL interface and via a standalone script runner interface.

## Getting started

Build the `reflex-cli` executable via `cargo`:

```shell
cargo build --release --workspace --package reflex-cli --bin reflex-cli
```

Once the `reflex-cli` executable has compiled successfully, it can be used in one of two modes.

### Option 1: Interactive mode

Run `reflex-cli` executable with no arguments to launch an interactive REPL session:

```shell
$ reflex-cli
> 1.2 + 3.4
4.6
> ((x, y) => x + y)(1.2, 3.4)
4.6
> exit
```
> The session can be terminated either by entering the `exit` REPL command or sending the `Ctrl+C` signal (`SIGINT`)

The default REPL syntax is a subset of JavaScript syntax, provided by the [ReflexJS](../reflex-js) package. Alternative syntaxes can be specified via the `--syntax` CLI argument:

```shell
$ reflex-cli --syntax lisp
> (+ 1.2 3.4)
4.6
> ((lambda (x y) (+ x y)) 1.2 3.4)
4.6
> exit
```

### Option 2: Script mode

For examples of using Reflex CLI to execute scripts directly, see the [`examples`](./examples) directory within this package.

For more information on the various `reflex-cli` CLI options, run the `reflex-cli --help` command.

## License

This software is distributed under the Apache 2.0 license. See the full [`LICENSE`](./LICENSE) text for details.
