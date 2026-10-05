# Query planning

The gateway plans subgraph calls for each operation, fetches any required entity fields, and combines the results into a GraphQL response.

## Explain a query plan

Use `explain` to see which subgraphs a query will call without executing it. For the [lookup example below](#connecting-objects-across-graphql-services):

```scala mdoc:invisible
import caliban.gateway.GatewayInterpreter

def interpreter: GatewayInterpreter[Any] = ???
```

```scala mdoc:compile-only
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

## Connecting objects across GraphQL services

Federation schemas already explain how to fetch an entity from another service. With GraphQL services, you provide that information using a `Lookup`.

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

```scala mdoc:invisible
val reviewsSdl = """
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
"""
```

```scala mdoc:silent
import caliban.gateway.{ Lookup, Subgraph }
import zio.http._

val reviews = Subgraph
  .graphql("reviews", url"http://reviews:8080/graphql", reviewsSdl)
  .withLookup(
    Lookup.list(
      "Product",
      "productsByIds",
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

- Results can arrive in any order: the gateway matches each returned object to its key through the key fields. Return non-null objects and omit missing ones.
- `Argument.key("id")` reads the `id` from the object that the gateway is fetching.
- `Argument.obj(...)` builds an input object for the subgraph.
- `Argument.batch(...)` builds one argument value for each requested object. `Lookup.list` reads keys only inside `Argument.batch`, and `Lookup.single` does not accept it. The compiler rejects both mistakes.

For a service that exposes `productById(id: ID!): Product`, use:

```scala mdoc:compile-only
Lookup.single(
  "Product",
  "productById",
  "id" -> Lookup.Argument.key("id")
)
```

Prefer a batch lookup wherever the subgraph supports one. It collapses several objects into a single subgraph request.

A subgraph whose SDL the gateway knows can declare a single lookup in its schema instead, with `@lookup`. Each argument maps to the field of the same name on the returned type:

```graphql
type Query {
  productById(id: ID!): Product @lookup
}
```

`@is` maps an argument to other fields with a field selection map: a path such as `address.id`, an input object such as `{ id sku }`, a list such as `parts[id]`, or a type condition such as `<Book>.isbn`. Alternatives separated by `|` declare several keys on one field, usually with a `@oneOf` input:

```graphql
type Query {
  person(by: PersonBy! @is(field: "{ id } | { addressId: address.id }")): Person @lookup
}

input PersonBy @oneOf {
  id: ID
  addressId: ID
}
```

The gateway picks, for each fetch, a lookup whose key the source of the objects can supply. A key field may be one that only another subgraph defines; the gateway then fetches it from there first. A lookup that returns an interface or a union resolves each possible type that one of its alternatives applies to. Lookup fields can also sit under argument-less fields of the query root, such as `Query.lookups.productById`, and under `@internal` ones.

Key fields resolved by several subgraphs must be declared as keys in each of them. Here, the catalog schema declares `type Product @key(fields: "id")` when it is pinned.

### Fields that require data from other services

A field can take arguments that the gateway fills with data from other services. `@require` names that data with a field selection map, as `@is` does. The map may also pass constant arguments, as in `weight(unit: IMPERIAL)`. Clients don't see these arguments:

```graphql
type Product @key(fields: "id") {
  id: ID!
  shippingEstimate(weight: Int! @require(field: "weight")): Int
}
```

The gateway fetches the required fields first. It then calls the service through one of its lookups and fills in the values for each object. If a value is missing, or null for a non-null argument, the gateway doesn't request that field for the object: the field is null and the response reports an error. A `Lookup.list` lookup takes one set of values per call, so objects with different values go in separate calls.

## Operation cache

The gateway reuses validated operations and query plans for repeated requests. Subgraph calls still fetch current data. Set the total estimated cache weight with `GatewayConfig.withMaxOperationCacheWeight`:

```scala mdoc:invisible
import caliban.gateway.Gateway

val catalog = Subgraph.graphql("catalog", url"http://catalog:8080/graphql")
val gateway = Gateway.compose(catalog, reviews)
```

```scala mdoc:compile-only
val bounded = gateway.withConfig(_.withMaxOperationCacheWeight(8L * 1024L * 1024L))
```

If you use `Configurator.setValidations`, reuse the same validation functions across requests. Creating fresh lambdas prevents cache reuse.

[Document resolution](hooks.md#persisted-and-trusted-documents) runs before cache lookup. [Authorization](hooks.md#authorizing-operations) runs before execution, including on cache hits. A resolution hook can disable caching for a request with `cacheable = false`.

## Supported operations

The gateway supports queries, mutations, and [subscriptions](subscriptions.md). It does not support `@defer` or `@stream` incremental responses from subgraphs.
