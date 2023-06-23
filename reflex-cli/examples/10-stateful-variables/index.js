import { hash, sequence } from 'reflex::core';
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

const token1 = hash('token1');
const token2 = hash('token2');
const token3 = hash('token3');

const value = sequence(counter.reset(3, token1), (_value1) =>
  sequence(counter.increment(token2), (_value2) =>
    sequence(counter.increment(token3), (_value3) => counter.value),
  ),
);

export default `Final value: ${value}`;
