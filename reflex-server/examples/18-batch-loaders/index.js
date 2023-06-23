import { Resolver } from 'reflex::graphql';
import { fetch } from 'reflex::http';
import DataLoader from 'reflex::loader';

const users = new DataLoader('User', (userIds) => {
  const params = userIds.map((id) => `id=${id}`).join('&');
  const url = `https://jsonplaceholder.typicode.com/users?${params}`;
  const results = fetch(url).json();
  return new Map(results.map((user) => [user.id, user]));
});

export default new Resolver({
  query: {
    user: ({ id }) => users.load(parseInt(id)),
  },
  mutation: null,
  subscription: null,
});
