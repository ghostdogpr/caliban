# Query planning

The gateway plans subgraph calls for each operation, fetches any required entity fields, and combines the results into a GraphQL response.

## Explain a query plan

Use `explain` to see which subgraphs a query will call without executing it. For the [lookup example below](#connecting-objects-across-ordinary-services):

```scala
val plan = interpreter.explain("""
  query {
    product(id: "p1") {
      name
      reviews { body }
    }
  }
""")
```

A plan for this query could look like:

```
query
fetch catalog at $.product fields [name, id (key)]
fetch reviews after catalog at $.product via Product(id) fields [reviews.body]
```

Each `fetch` names a subgraph. `$.product` is the location in the client response. `(key)` marks a field needed for a later lookup, even if the client did not request it. `after catalog` means that reviews must wait for the catalog result.

## Connecting objects across ordinary services

Federation schemas already explain how to fetch an entity from another service. With ordinary GraphQL services, you provide that information using a `Lookup`.

Suppose the catalog service exposes this schema:

```graphql
type Query {
  product(id: ID!): Product
}

type Product {
  id: ID!
  name: String!
}
```

The reviews service adds `reviews` to `Product` and exposes a batch lookup:

```graphql
type Query {
  productsByIds(ids: [ID!]!): [Product!]!
}

type Product {
  id: ID!
  reviews: [Review!]!
}

type Review {
  body: String!
}
```

Use the reviews schema above as `reviewsSdl`, and describe how the service fetches products:

```scala
import caliban.gateway.{ Lookup, Subgraph }
import zio.http._

val reviews = Subgraph
  .graphql("reviews", url"http://reviews:8080/graphql", reviewsSdl)
  .withLookup(
    Lookup.list(
      "Product",
      List("id"),
      "productsByIds",
      Map("id" -> "id"),
      "ids" -> Lookup.Argument.batch(Lookup.Argument.key("id"))
    )
  )
```

Compose the catalog and reviews subgraphs. A client can then ask:

```graphql
query {
  product(id: "p1") {
    name
    reviews { body }
  }
}
```

The gateway fetches `product` from the catalog, including `id` even though the client did not request it. It then calls `productsByIds(ids: ["p1"])` on reviews and attaches the returned reviews to that product. The lookup belongs on the service being called, which is reviews here.

Use `Lookup.single` when the subgraph fetches one object at a time. Use `Lookup.list` when it accepts several keys in one request:

- The correlation map maps returned fields to key fields. `Map("id" -> "id")` matches a returned product's `id` to the requested key. Results can arrive in any order. Return non-null objects and omit missing ones.
- `Argument.key("id")` reads the `id` from the object that the gateway is fetching.
- `Argument.obj(...)` builds an input object for the subgraph.
- `Argument.batch(...)` builds one argument value for each requested object.

For a service that exposes `productById(id: ID!): Product`, use:

```scala
Lookup.single(
  "Product",
  List("id"),
  "productById",
  "id" -> Lookup.Argument.key("id")
)
```

Prefer a batch lookup wherever the subgraph supports one. It collapses several objects into a single subgraph request.

## Operation cache

The gateway reuses validated operations and query plans for repeated requests. Subgraph calls still fetch current data. Set the total estimated cache weight with `GatewayConfig.withMaxOperationCacheWeight`:

```scala
val bounded = gateway.withConfig(_.withMaxOperationCacheWeight(8L * 1024L * 1024L))
```

If you use `Configurator.setValidations`, reuse the same validation functions across requests. Creating fresh lambdas prevents cache reuse.

[Document resolution](hooks.md#persisted-and-trusted-documents) runs before cache lookup. [Authorization](hooks.md#authorizing-operations) runs before execution, including on cache hits. A resolution hook can disable caching for a request with `cacheable = false`.

## Supported operations

The gateway supports queries, mutations, and [subscriptions](subscriptions.md). It does not support `@defer` or `@stream` incremental responses from subgraphs.
