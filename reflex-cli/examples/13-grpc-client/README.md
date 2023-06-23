# Reflex CLI Example: gRPC client

This example demonstrates how to load external gRPC data within Reflex.

In this example, the data is loaded from the public [grpcbin](https://grpcb.in/) dummy gRPC API.

To communicate with an external gRPC API from within ReflexJS, you must import the service descriptor set for the external gRPC API into the ReflexJS module.

To generate the service descriptor set from a `.proto` file containing a `service` definition, invoke the [`protoc`](https://grpc.io/docs/protoc-installation/) Protocol Buffer Compiler with the `--descriptor_set_out` argument.

For example, you can generate the service descriptor set for a service defined in `hello.proto` as follows:

```shell
protoc --include_imports \
  --descriptor_set_out ./hello.proto.bin \
  ./hello.proto
```

The resulting `hello.proto.bin` file can then be imported directly into a ReflexJS module (the `.proto.bin` extension indicates to ReflexJS that this import should be parsed as a gRPC service descriptor set).

Importantly, to communicate with the API, the Reflex runtime must be invoked with a `--grpc-service` argument containing the path to the `.proto.bin` service descriptor set (see usage instructions below).

When imported into a ReflexJS module, the `.proto.bin` service descriptor module exposes named exports for each service defined within the `.proto`, where each named service export is a constructor for a gRPC client that can be used within ReflexJS to communicate with that API.

As seen in the example, the gRPC client constructor is instantiated with an object containing a `url` field that denotes the URL of the external gRPC service to connect to. The URL must specify either the `http://` or `https://` protocol, depending on whether the the remote service is TLS-enabled.

Once instantiated, gRPC calls can be made on the resulting API client via its named methods, where the method names are taken verbatim from the `rpc` method definitions within the relevant service definition in the `.proto` schema. Calling the API client methods returns the parsed gRPC response for the given call.

The gRPC client methods take 2 arguments:

- `message`: Message object to send to the gRPC server as the argument for the method call (the field names must match those defined in the protobuf schema)
- `options`: Object containing the following fields:
    - `token`: Idempotency token for this operation

Note how in ReflexJS, there is no need to await asynchronous data fetches: you can access the response data payload as if it were synchronously available, with evaluation proceeding as soon as the data arrives.

While this example demonstrates a unary call that returns a single response, exactly the same approach is used for server streaming calls that return multiple responses.

When performing a server streaming call, the resulting value will change whenever a new result payload is received.

## Source files

- [`index.js`](./index.js)

## Running the example

```shell
$ reflex-cli --grpc-service ./hello.proto.bin ./index.js
```

## Example output

```
"Greeting from server: hello Reflex"
```
