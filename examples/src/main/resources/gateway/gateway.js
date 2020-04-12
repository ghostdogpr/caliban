const {ApolloServer} = require('apollo-server');
const {ApolloGateway} = require('@apollo/gateway');

const gateway = new ApolloGateway({
  serviceList: [
    { name: 'episodes', url: 'http://localhost:8088/api/graphql'},
    { name: 'characters', url: 'http://localhost:8089/api/graphql' }
  ],
  debug: true
});

(async () => {
  const server = new ApolloServer({
    gateway,
    subscriptions: false
  });

  server.listen().then(({url}) => {
    console.log(`🚀 Server ready at ${url}`)
  })
})();