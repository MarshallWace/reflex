import { fetch } from 'reflex::http';
import DataLoader from 'reflex::loader';

const users = new DataLoader('User', (userIds) => {
  const params = userIds.map((id) => `id=${id}`).join('&');
  const url = `https://jsonplaceholder.typicode.com/users?${params}`;
  const results = fetch(url).json();
  return new Map(results.map((user) => [user.id, user]));
});

const user1 = users.load(parseInt(1));
const user3 = users.load(parseInt(3));

export default `Hello, ${user1.name} and ${user3.name}!`;
