import { fetch } from 'reflex::http';

const message = (() => {
  try {
    return fetch('http://@@@').json();
  } catch (error) {
    return error.message;
  }
})();

export default message;
