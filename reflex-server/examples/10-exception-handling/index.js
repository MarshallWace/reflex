import { Resolver } from 'reflex::graphql';
import { fetch } from 'reflex::http';

const message = (() => {
  try {
    return fetch('http://@@@').json();
  } catch (error) {
    return error.message;
  }
})();

export default new Resolver({
  query: {
    message,
  },
  mutation: null,
  subscription: null,
});
