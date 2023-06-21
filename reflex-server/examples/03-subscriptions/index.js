import { Resolver } from 'reflex::graphql';
import { timestamp } from 'reflex::time';

// Current timestamp in milliseconds (sampled every 1000 milliseconds)
const now = timestamp({ interval: 1000 });

export default new Resolver({
  query: null,
  mutation: null,
  subscription: {
    now: `Current UNIX time: ${Math.floor(now / 1000)}`,
  },
});
