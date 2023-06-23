import { Resolver } from 'reflex::graphql';

// Import the gRPC service definition set
import { HelloService } from './hello.proto.bin';

// Create a new API client for the imported gRPC service
const api = new HelloService({ url: 'https://grpcb.in:443' });

export default new Resolver({
  query: {
    greeting: ({ name }) => {
      // Call a gRPC method on the remote service
      const { reply } = api.SayHello(
        { greeting: name },
        // In a real application, an idempotency token should be provided
        { token: null },
      );
      return `Greeting from server: ${reply}`;
    },
  },
  mutation: null,
  subscription: null,
});
