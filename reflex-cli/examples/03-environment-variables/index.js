const username = 'APP_USER' in process.env ? process.env.APP_USER : null;

export default `Hello, ${username || 'world'}!`;
