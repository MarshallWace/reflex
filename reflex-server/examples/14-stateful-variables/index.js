import { hash, sequence } from 'reflex::core';
import { Resolver } from 'reflex::graphql';
import { get, set, increment } from 'reflex::state';
import { log } from 'reflex::utils';

const getCounter = (label) => {
  const uid = hash('counter', label);
  const initialValue = 0;
  return {
    value: get(uid, initialValue),
    increment: (token) =>
      sequence(increment(uid, token), (result) =>
        log(result, `[increment ${label}]`),
      ),
    reset: (value, token) =>
      sequence(set(uid, parseInt(value), token), (result) =>
        log(result, `[reset ${label}]`),
      ),
  };
};

const counter = getCounter('foo');

export default new Resolver((requestToken) => ({
  query: {
    value: counter.value,
  },
  mutation: {
    increment: counter.increment(requestToken),
    reset: ({ value }) => counter.reset(value, requestToken),
  },
  subscription: {
    value: counter.value,
  },
}));
