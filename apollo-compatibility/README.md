# Federated subgraph to test apollo federation spec compatibility

Implementation of a federated subgraph aligned to the requirements outlined in [apollo-federation-subgraph-compatibility](https://github.com/apollographql/apollo-federation-subgraph-compatibility).

The subgraph can be used to verify compability against [Apollo Federation Subgraph Specification](https://www.apollographql.com/docs/federation/subgraph-spec).

### Run compatibility tests
Execute the following command from the root of the repo

```
npx @apollo/federation-subgraph-compatibility docker --compose apollo-compatibility/docker-compose.yml --schema apollo-compatibility/schema.graphql
```

### Printing the GraphQL Schema (SDL)

```
sbt "apollo-compability/run printSchema"
```