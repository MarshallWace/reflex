import { Resolver } from 'reflex::graphql';
import { fetch } from 'reflex::http';

export default new Resolver({
  query: {
    user: ({ id }) =>
      fetch(`https://jsonplaceholder.typicode.com/users/${id}`).json(),
  },
  mutation: null,
  subscription: null,
});
