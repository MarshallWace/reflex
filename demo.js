// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
import { Resolver } from 'reflex::graphql';

export default new Resolver({
  query: (uid) => ({
    foo: `hi: ${uid}`
  }),
  mutation: null,
  subscription: null,
});
