import { ApolloServer } from '@apollo/server';
import { ApolloServerPluginInlineTrace } from '@apollo/server/plugin/inlineTrace'
import { ApolloGateway, IntrospectAndCompose } from '@apollo/gateway';
import {startStandaloneServer} from "@apollo/server/standalone";

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

  const { url } = await startStandaloneServer(server, {
    listen: { port: 4000 }
  })

  console.log(`🚀 Server ready at ${url}`)
})();