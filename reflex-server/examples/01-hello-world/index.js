import { Resolver } from 'reflex::graphql';

export default new Resolver({
  query: {
    hello: 'Hello, world!',
  },
  mutation: null,
  subscription: null,
});
