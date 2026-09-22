# Subgraphs

Give every subgraph a unique name such as `products` or `reviews`. Caliban uses it in error messages and monitoring data.

Choose `Subgraph.graphql` for ordinary GraphQL composition or `Subgraph.federation` for Federation composition. Both accept a remote URL or a local Caliban `GraphQL` API.

## Ordinary GraphQL services

Use `Subgraph.graphql` for a regular GraphQL endpoint:

```scala
import caliban.gateway.{ Gateway, Subgraph }
import zio.http._

val catalog = Subgraph.graphql(
  "catalog",
  url"http://catalog:8080/graphql"
)
```

By default, the gateway acquires the schema through introspection. You can instead provide SDL directly:

```scala
val catalog = Subgraph.graphql(
  "catalog",
  url"http://catalog:8080/graphql",
  """
    type Query {
      product(id: ID!): Product
    }

    type Product {
      id: ID!
      name: String!
    }
  """
)
```

Pin SDL when introspection is unavailable. Keep it consistent with the deployed schema. The service must still be reachable to execute requests.

## Federation subgraphs

Use `Subgraph.federation` for an Apollo Federation subgraph:

```scala
val products = Subgraph.federation(
  "products",
  url"http://products:8080/graphql"
)

val reviews = Subgraph.federation(
  "reviews",
  url"http://reviews:8080/graphql"
)

val gateway = Gateway.compose(products, reviews)
```

The gateway acquires each Federation schema through `_service`. To pin a Federation schema, pass its SDL as the third argument to `Subgraph.federation`.

## Composition rules

The gateway combines root fields into one public schema and merges types by name. A `Product` declared in two services describes the same public type. Rename a type before composition if the names match but the concepts differ.

- Ordinary services cannot both own the same root field, such as `Query.product`. Rename or hide one field with a [schema transformation](#shaping-the-public-schema).
- Object types can contribute different fields. Shared fields need compatible output types and arguments. For example, `Product.price: Int` and `Product.price: String` conflict. Output nullability can differ. The composed field is nullable if a contributing source allows null.
- Input fields must agree on their types and defaults. A required input field must exist in every service that declares that input type.
- Federation 2 fields with multiple owners need compatible `@shareable` declarations or an applicable `@override`. Subscription root fields always need one owner.

Merging types does not tell the gateway how to retrieve an object from another service. Ordinary services need a [lookup](planning.md#connecting-objects-across-ordinary-services) for that step. Federation schemas supply entity keys and routing information themselves.

Composition errors identify the field or type and the services involved. Fix incompatible definitions at the source, or transform the schema before composing it. Choosing a different subgraph order does not resolve a conflict.

## Supergraphs

A supergraph document contains the combined schema and the routing information for its services. If you already have one for Apollo Router or Hive Router, load it with `Gateway.fromSupergraph`:

```scala
import zio.Config.Secret
import zio.http._
import caliban.gateway.{ Gateway, Supergraph }

// From a file
val file = Supergraph.file(java.nio.file.Paths.get("supergraph.graphql"))

// From an HTTP URL
val http = Supergraph.http(url"https://example.com/supergraph.graphql")

// From a raw SDL string; `Supergraph.parsed` takes an already parsed Caliban document
val sdl = Supergraph.sdl("<a raw supergraph sdl string>")

// From the Apollo schema registry
val apollo = Supergraph.uplink("my-graph@production", Secret("service:my-apikey"))

// From the Hive CDN
val hive = Supergraph.hive("my-target-id", Secret("my-cdn-key"))

val gateway = Gateway.fromSupergraph(apollo)
```

### Service URLs and credentials

By default, the gateway uses each service URL from the supergraph and `RemoteGraphQLConfig.default`. Configure services by their subgraph names, such as `products`, with `withSubgraphConfig`. Override URLs with `withSubgraphEndpoint`:

```scala
import caliban.gateway.{ RemoteGraphQLConfig, Supergraph }
import zio.http._

val source = Supergraph.file(java.nio.file.Paths.get("supergraph.graphql"))
  .withSubgraphConfig {
    case "products" => RemoteGraphQLConfig.default.withExecution(
      _.forwardIncomingHeaders("Authorization")
    )
    case _ => RemoteGraphQLConfig.default
  }
  .withSubgraphEndpoint {
    case "products" => Some(url"http://localhost:8081/graphql")
    case _          => None
  }

val gateway = Gateway.fromSupergraph(source)
```

`None` keeps the URL declared in the supergraph. The service configuration controls execution, including headers, retries, and subscriptions. Its acquisition settings are unused because the schemas come from the supergraph. Set authentication for downloading an HTTP supergraph on `Supergraph.http` itself:

```scala
val source = Supergraph.http(
  url"https://example.com/supergraph.graphql",
  RemoteGraphQLConfig.Acquisition.default
    .withHeaders(Header.Authorization.Bearer("schema-token"))
)
```

Registry credentials passed to `Supergraph.uplink` or `Supergraph.hive` authenticate schema downloads. They are not forwarded to subgraphs.

## Progressive field overrides

Federation 2.7 and later can move a field between subgraphs gradually. Add a percentage label to `@override` to let the gateway select the overriding subgraph for that share of requests:

```graphql
type Product {
  inStock: Boolean @override(from: "inventory", label: "percent(10)")
}
```

`percent(10)` routes roughly 10% of requests to the overriding service. The choice is random per request, not tied to a user or session. Fields with the same label share the choice.

For custom labels, use an [override-label hook](hooks.md#progressive-override-labels).

## In-process Caliban APIs

Pass a Caliban API to `Subgraph.graphql` to execute it in process:

```scala
import caliban._
import caliban.gateway.Subgraph
import caliban.schema.GenericSchema

object LocalApi extends GenericSchema[Any] {
  import auto._

  final case class Query(gatewayVersion: String)

  val api = graphQL(RootResolver(Query("v1")))
}

val local = Subgraph.graphql("gateway", LocalApi.api)
```

For a local Federation API, add the [`caliban-federation` dependency](../federation.md#dependencies) and configure the API before passing it to `Subgraph.federation`:

```scala
import caliban.federation.v2_6.federated

val federatedApi = LocalApi.api @@ federated
val localFederation = Subgraph.federation("gateway", federatedApi)
```

`Subgraph.federation` expects an API that already provides Federation resolvers. It does not add them. You can mix local and remote subgraphs in one `Gateway.compose` call.

### Providing local dependencies

A local API can require ZIO services. Those dependencies belong to request execution, so building the gateway interpreter does not require them. Provide application-wide services around the server:

```scala
import caliban.{ graphQL, QuickAdapter, RootResolver }
import caliban.gateway.{ Gateway, Subgraph }
import caliban.schema.GenericSchema
import zio._

trait Catalog {
  def productCount: UIO[Int]
}

object CatalogApi extends GenericSchema[Catalog] {
  import auto._

  final case class Query(productCount: URIO[Catalog, Int])
  val api = graphQL(RootResolver(
    Query(ZIO.serviceWithZIO[Catalog](_.productCount))
  ))
}

def serveCatalog(catalogLayer: ULayer[Catalog]) = ZIO.scoped {
  for {
    interpreter <- Gateway.compose(Subgraph.graphql("catalog", CatalogApi.api)).interpreter
    _           <- QuickAdapter(interpreter)
                     .runServer(4000, "/graphql")
                     .provideLayer(catalogLayer)
  } yield ()
}
```

Supply request-specific dependencies, such as verified user claims, in the [HTTP request handler](hooks.md#connecting-http-authentication).

## Shaping the public schema

Transform a subgraph before composition to rename or hide fields. For a service with `Product.reviews` and `Product.internalScore`:

```scala
import caliban.gateway.SchemaTransformation
import zio.http._

val reviews = Subgraph
  .graphql("reviews", url"http://reviews:8080/graphql", reviewsSdl)
  .transform(
    SchemaTransformation.renameField("Product", "reviews", "customerReviews"),
    SchemaTransformation.hideField("Product", "internalScore")
  )
```

Clients see the new names. The remote service still receives the original ones.

You can rename types, fields, and arguments, and hide types, fields, optional arguments, and optional input fields. Enum values and input-field names stay as they are. Invalid or conflicting changes fail at startup.

To set authentication, timeouts, or retries for a remote subgraph, see [Configuration](configuration.md#configuring-remote-services).
