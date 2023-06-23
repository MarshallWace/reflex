import { hash } from 'reflex::core';
import { fetch, Request } from 'reflex::http';
import { backoff, poll, retryErrors } from 'reflex::invalidation';

const POLL_INTERVAL = 1000; // Re-fetch a new result every 1000ms
const REQUEST_TIMEOUT = 30 * 1000; // Retry any requests that take longer than 30s to return a result

const API_URL = 'https://worldtimeapi.org/api/timezone/Etc/UTC';

const { unixtime } = retryErrors(
  { delay: backoff, timeout: REQUEST_TIMEOUT },
  (retryToken) =>
    poll(POLL_INTERVAL, (pollToken) =>
      fetch(
        new Request({
          method: 'GET',
          url: API_URL,
          headers: {},
          body: null,
          token: hash(pollToken, retryToken),
        }),
      ).json(),
    ),
);

export default `Current UNIX time: ${unixtime}`;
