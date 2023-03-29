# Federation

**Federation** is an optional module which can be included in your configuration to enroll with a federated schema.

## Dependencies

`caliban-federation` only depends on `caliban-core` and is very unobtrusive.

To use, add the following dependency to your `build.sbt` file:

```scala
"com.github.ghostdogpr" %% "caliban-federation" % "2.1.0"
```

## Federating

Federation allows graphs to become part of a larger graph without having to share models or create brittle
schema stitching code at the gateway level.

You can read more about federation and why it may be useful [here](https://www.apollographql.com/docs/apollo-server/federation/introduction/).

Federation creates a wrapper over your existing schema so that it can add the necessary hooks to support
interaction with the gateway.

If you already have a graph you can add federation simply by adding the `federated` annotation:

```scala
import caliban.federation.v1._

val schema: GraphQL[R] = graphQL(RootResolver(Queries(
  characters = List(Character("Amos"))
)))

val federatedSchema: GraphQL[R] = schema @@ federated
```

This will wrap the bare minimum schema additions around your API so that the gateway will recognize your schema.
To actually enable entity resolution you will need to do a bit of leg work.

First, any types that will be "resolvable" need to be annotated with a `@key` directive. You can use a helper function found
in the `federation` package to help with that. 

```scala
@GQLKey("name")
case class Character(name: String)
```

The `"name"` field is a field selector minus the outer braces. 

If you need to extend a type from another service, you will need to define a stub version of it in the current service
and annotate it with the `@extends` annotation

```scala
@GQLKey("season episode") 
@GQLExtend
case class Episode(@GQLExternal season: Int, @GQLExternal episode: Int, cast: List[Character])
```

Note the additional annotations we needed in this case. `Extend` is needed to tell the gateway that this type is defined within
another service, while the `External` flags these fields as being owned by the other service (there are several other annotations
available that you are encouraged to read about).

Once you have annotated your types, you need to tell `Federation` how to resolve those types. Federation uses a slightly
different mechanism in resolving types from a standard GraphQL query, so for each type that you wish to support, you will
need to add an `EntityResolver`:

```scala
EntityResolver[CharacterService, CharacterArgs, Character](args => 
  ZQuery.fromEffect(characters.getCharacter(args.name))
)  
```

`EntityResolvers` like normal field resolvers also supports a "metadata" variant which can be used to inspect the requested
fields and potentially optimize the resulting query. You can use the provided helper method if you need to access the metadata field:

```scala
EntityResolver.fromMetadata[CharacterArgs](field => args => {
  if (field.fields.forall(_.name == "name")) ZQuery.succeed(Character(args.name, Nil, None))
  else ZQuery.fromEffect(characters.getCharacter(args.name))
})
```

In the above we need to define an resolver which takes an `R` environment type,
an `A` which has an implicit `ArgBuilder` and an `Option[Out]` where `Out` has an implicit
`Schema[R, Out]` available. Creating the above we can now add these resolvers to our federated schema like so:

```scala
schema @@ federated(aResolver, additionalResolvers:_*)
```

You can now use the resulting `GraphQL[R]` to start querying. You can also see the full code example [here](https://github.com/ghostdogpr/caliban/tree/series/2.x/examples/src/main/scala/example/federation)

## Tracing

Federated tracing is slightly different from standard apollo-tracing thus it comes with its own wrapper defined in the `caliban-federation` module.

```scala
import caliban.federation.tracing.ApolloFederatedTracing


val api = schema @@ federated(resolver, additionalResolvers: _*) @@ ApolloFederatedTracing.wrapper
```
In federated tracing the gateway communicates with the implementing service via a header `apollo-federation-include-trace`,
for now the only value it can send is `ftv1`. Thus if you detect this header then you should enable tracing otherwise you can disable it.

If you are using one of the wrappers you are done, they will automatically detect when the gateway
enables tracing on a request. However, if you are calling the `interpreter.execute` independently or you have some other custom
set up you will need to add one more step to enable tracing.

If you wish to enable it manually (after detecting the header with your preferred framework) you can call: `request.withFederatedTracing` which will return a new `GraphQLRequest` that informs the wrapper
that it should include tracing data as part of the response extensions.


## Federation V2

Caliban can support the v2 federation specification as well. If your gateway supports the [Federation V2 specification](https://www.apollographql.com/docs/federation/federation-spec), you can specify the supported feature set
by using `caliban.federation.v2_x` where `x` is the minor version of the specification you wish to use.

| Directive    | Caliban Type | Version | Caliban package
--------------| --------------|---------| ---------------
| `@shareable` | `@GQLShareable` | v2.0    | `caliban.federation.v2_0`
| `@inaccessable` | `@GQLInaccessible` | v2.0    | `caliban.federation.v2_0`
| `@override`  | `@GQLOverride` | v2.0    | `caliban.federation.v2_0`
| `@tag`       | `@GQLTag` | v2.0    | `caliban.federation.v2_0`
| `@composeDirective` | `ComposeDirective` | v2.1    | `caliban.federation.v2_1`
| `@interfaceObject` | `@GQLInterfaceObject` | v2.3    | `caliban.federation.v2_3`

The `GQLKey` field now also supports the `resolvable` argument. 

Using the new `federated` aspect from any v2_x package will automatically make your graph available as a v2 schema,
even if you aren't using the new directives.

For more information see the [Federation V2 specification](https://www.apollographql.com/docs/federation/federation-2/new-in-federation-2/).

### Customizing Federation

Federation 2.1 introduced a new schema level directive called `@composeDirective` which allows you to specify custom directives that should
be visible to clients of the gateway (by default all directives are hidden to clients of the gateway)

GraphQL federation is an evolving specification and not all routers support all features.
Caliban provides support for `v2.0`, `v2.1` and `v2.3` of the specification. If you need to use
an earlier version or you need to customize some aspect of the federation directives (for instance by providing your own `@composeDirective`s) you can do so by simply extending the `FederationV2` class.

```scala
// With a package object but you can also create a normal object
package object myFederation extends FederationV2(
  Versions.v2_3 :: 
    Link("https://myspecs.dev/myDirective/v1.0", List(
      Import("@myDirective"),
      Import("@anotherDirective", as = Some("@hello"))
    )) :: 
      ComposeDirective("@myDirective") :: 
      ComposeDirective("@hello") :: Nil
    )
 with FederationDirectivesV2_3

// Then import your new federation object instead of `caliban.federation.v2_3`
import myFederation._
```