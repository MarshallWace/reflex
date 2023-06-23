import { Resolver } from 'reflex::graphql';

const username = 'APP_USER' in process.env ? process.env.APP_USER : null;

export default new Resolver({
  query: {
    hello: `Hello, ${username || 'world'}!`,
  },
  mutation: null,
  subscription: null,
});
