# Federation

**Federation** is an optional module which can be included in your configuration to enroll with a federated schema.

## Dependencies

`caliban-federation` only depends on `caliban-core` and is very unobtrusive.

To use, add the following line to your `build.sbt` file:

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-federation" % "0.8.0"
```

## Federating

Federation allows graphs to become part of a larger graph without having to share models or create brittle
schema stitching code at the gateway level.

You can read more about federation and why it may be useful [here](https://www.apollographql.com/docs/apollo-server/federation/introduction/).

Federation creates a wrapper over your existing schema so that it can add the necessary hooks to support
interaction with the gateway.

If you already have a graph you can add federation simply by calling the wrapper function `federate`

```scala
import caliban.federation._

val schema: GraphQL[R] = graphQL(RootResolver(Queries(
  characters = List(Character("Amos"))
)))

val federatedSchema: GraphQL[R] = federate(schema)
```

This will wrap the bare minimum schema additions around your API so that the gateway will recognize your schema.
To actually enable entity resolution you will need to do a bit of leg work.

First, any types that will be "resolvable" need to be annotated with a `@key` directive. You can use a helper function found
in the `federation` package to help with that. 

```scala
@GQLDirective(Key("name"))
case class Character(name: String)
```

The `"name"` field is a field selector minus the outer braces. 

If you need to extend a type from another service, you will need to define a stub version of it in the current service
and annotate it with the `@extends` annotation

```scala
@GQLDirective(Key("season episode")) 
@GQLDirective(Extend)
case class Episode(@GQLDirective(External) season: Int, @GQLDirective(External) episode: Int, cast: List[Character])
```

Note the additional annotations we needed in this case. `Extend` is needed to tell the gateway that this type is defined within
another service, while the `External` flags these fields as being owned by the other service (there are several other annotations
available that you are encouraged to read about).

Once you have annotated your types you need to tell `Federation` how to resolve those types. Federation uses a slightly
different mechanism in resolving types from a standard GraphQL query, so for each type that you wish to support, you will
need to add an `EntityResolver`:

```scala
EntityResolver[CharacterService, CharacterArgs, Character](args => 
  ZQuery.fromEffect(characters.getCharacter(args.name))
)  
```

In the above we need to define an resolver which takes an `R` environment type,
an `A` which has an implicit `ArgBuilder` and an `Option[Out]` where `Out` has an implicit
`Schema[R, Out]` available. Creating the above we can now add these resolvers to our federated schema like so:

```scala
federate(schema, aResolver, additionalResolvers:_*)
```

You can now use the resulting `GraphQL[R]` to start querying. You can also see the full code example [here](https://github.com/ghostdogpr/caliban/tree/master/examples/src/main/scala/caliban/federation)
