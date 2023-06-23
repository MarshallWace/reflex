import { Resolver } from 'reflex::graphql';
import { fetch, Request } from 'reflex::http';

export default new Resolver((requestToken) => ({
  query: null,
  mutation: {
    user: ({ id }) => ({
      setName: ({ value }) =>
        fetch(
          new Request({
            method: 'PATCH',
            url: `https://jsonplaceholder.typicode.com/users/${id}`,
            headers: {
              'Content-type': 'application/json; charset=UTF-8',
            },
            body: JSON.stringify({ name: value }),
            token: requestToken,
          }),
        ).json(),
    }),
  },
  subscription: null,
}));
