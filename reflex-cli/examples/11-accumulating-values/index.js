import { scan } from 'reflex::state';
import { now } from 'reflex::time';

// Current timestamp in milliseconds (sampled every 1000 milliseconds)
const timestamp = now({ interval: 1000 });

// Calculate the distribution of the final digits of each emitted timestamp
const buckets = scan(
  // Input expression
  timestamp,
  // Seed value
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  // Reducer function
  (state, value) => {
    const buckets = state;
    const bucket = value % 10;
    return [
      ...buckets.slice(0, bucket),
      buckets[bucket] + 1,
      ...buckets.slice(bucket + 1, buckets.length),
    ];
  },
);

export default `Timestamp digit counts: ${buckets.join(', ')}`;
