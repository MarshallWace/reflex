// Import the GraphQL schema
import CountriesApi from './countries.graphql';

// Create a new API client for the imported GraphQL schema
const api = new CountriesApi({
  url: 'https://countries.trevorblades.com/graphql',
});

const getCountryData = (countryCode) => {
  const { country } = api.execute({
    query: /* GraphQL */ `
      query CapitalCity($country: ID!) {
        country(code: $country) {
          name
          capital
        }
      }
    `,
    operationName: 'CapitalCity',
    variables: {
      country: countryCode,
    },
    // In a real application, an idempotency token should be provided
    token: null,
  });
  return country;
};

const country = getCountryData('GB');

export default `Capital of ${country.name}: ${country.capital}`;
