const {ApolloServer} = require('apollo-server');
const {ApolloServerPluginInlineTrace} = require('apollo-server-core')
const {ApolloGateway, IntrospectAndCompose} = require('@apollo/gateway');

const gateway = new ApolloGateway({
  supergraphSdl: new IntrospectAndCompose({
    subgraphs: [
      { name: 'episodes', url: 'http://localhost:8088/api/graphql'},
      { name: 'characters', url: 'http://localhost:8089/api/graphql' }
    ]
  }),
  debug: true
});

(async () => {
  const server = new ApolloServer({
    gateway,
    subscriptions: false,
    plugins: [
      ApolloServerPluginInlineTrace()
    ]
  });

  server.listen().then(({url}) => {
    console.log(`🚀 Server ready at ${url}`)
  })
})();