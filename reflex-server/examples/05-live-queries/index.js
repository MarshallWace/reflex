import { Resolver } from 'reflex::graphql';
import { now } from 'reflex::time';

// Current timestamp in milliseconds (sampled every 1000 milliseconds)
const timestamp = now({ interval: 1000 });

// The same graph root will be used for both query and subscription operation roots
const root = {
  now: `Current UNIX time: ${Math.floor(timestamp / 1000)}`,
  millis: timestamp,
  sampled: ({ interval }) => {
    // Emits a new result every `interval` milliseconds
    const sampled = now({ interval });
    // Graph roots can be arbitrarily complex/nested for all operation types
    return {
      millis: sampled,
      seconds: sampled / 1000,
      labeled: ({ prefix }) => ({
        millis: `${prefix}: ${sampled}`,
        seconds: `${prefix}: ${sampled / 1000}`,
      }),
    };
  },
};

export default new Resolver({
  query: root,
  mutation: null,
  subscription: root,
});
