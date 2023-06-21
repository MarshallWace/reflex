import { Resolver } from 'reflex::graphql';

import localize from './localize.js';

import LOCALE_EN from './locale-en.json';
import LOCALE_ES from './locale-es.json';

const LOCALES = {
  en: LOCALE_EN,
  es: LOCALE_ES,
};

export default new Resolver({
  query: {
    greeting: ({ lang }) => localize(LOCALES[lang], 'HELLO'),
  },
  mutation: null,
  subscription: null,
});
