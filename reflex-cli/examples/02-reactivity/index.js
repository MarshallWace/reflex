import { now } from 'reflex::time';

// Current timestamp in milliseconds (sampled every 1000 milliseconds)
const timestamp = now({ interval: 1000 });

export default `Current UNIX time: ${Math.floor(timestamp / 1000)}`;
