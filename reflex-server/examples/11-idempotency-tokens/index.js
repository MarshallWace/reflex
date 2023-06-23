import { hash } from 'reflex::core';
import { Resolver } from 'reflex::graphql';
import { fetch, Request } from 'reflex::http';
import { now } from 'reflex::time';

// Public API that provides the current time in JSON format
const API_URL = 'https://worldtimeapi.org/api/timezone/Etc/UTC';

// Opaque idempotency token that changes every 1000ms
const token = hash(now({ interval: 1000 }));

const { unixtime } = fetch(
  new Request({ method: 'GET', url: API_URL, headers: {}, body: null, token }),
).json();

export default new Resolver({
  query: null,
  mutation: null,
  subscription: {
    now: `Current UNIX time: ${unixtime}`,
  },
});
