import { Resolver } from 'reflex::graphql';

export default new Resolver({
  query: {
    hello: ({ name }) => `Hello, ${name}!`,
  },
  mutation: null,
  subscription: null,
});
