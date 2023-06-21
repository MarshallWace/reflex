import { fetch } from 'reflex::http';

const user = fetch('https://jsonplaceholder.typicode.com/users/1').json();

export default `Hello, ${user.name}!`;
