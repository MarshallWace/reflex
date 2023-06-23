import { ifPending } from 'reflex::core';
import { Resolver } from 'reflex::graphql';
import { fetch } from 'reflex::http';

const LOADING_PLACEHOLDER = 'Loading...';

export default new Resolver({
  query: null,
  mutation: null,
  subscription: {
    user: ({ id }) => {
      const user = fetch(
        `https://jsonplaceholder.typicode.com/users/${id}`,
      ).json();
      return ifPending(user, {
        id,
        name: LOADING_PLACEHOLDER,
        username: LOADING_PLACEHOLDER,
        email: LOADING_PLACEHOLDER,
        address: {
          street: LOADING_PLACEHOLDER,
          suite: LOADING_PLACEHOLDER,
          city: LOADING_PLACEHOLDER,
          zipcode: LOADING_PLACEHOLDER,
          geo: {
            lat: LOADING_PLACEHOLDER,
            lng: LOADING_PLACEHOLDER,
          },
        },
        phone: LOADING_PLACEHOLDER,
        website: LOADING_PLACEHOLDER,
        company: {
          name: LOADING_PLACEHOLDER,
          catchPhrase: LOADING_PLACEHOLDER,
          bs: LOADING_PLACEHOLDER,
        },
      });
    },
  },
});
