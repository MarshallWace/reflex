# Reflex CLI Example: Importing modules

This example demonstrates how to import source modules within Reflex.

Note how in ReflexJS, the file extension must be present on the import path, as this determines which module loader to use.

## Source files

- [`index.js`](./index.js)
- [`locale-en.json`](./locale-en.json)
- [`locale-es.json`](./locale-es.json)
- [`localize.js`](./localize.js)

## Running the example

```shell
$ LOCALE=es reflex-cli ./index.js
```

## Example output

```
"¡Hola, mundo!"
```
