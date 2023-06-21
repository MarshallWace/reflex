import { timestamp } from 'reflex::time';

// Current timestamp in milliseconds (sampled every 1000 milliseconds)
const now = timestamp({ interval: 1000 });

export default `Current UNIX time: ${Math.floor(now / 1000)}`;
