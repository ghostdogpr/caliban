# Tools

Caliban comes with a module called `caliban-tools` that exposes some useful features:
- all the code generation features from `caliban-codegen-sbt`, so that you can use them without sbt: see `caliban.tools.Codegen`.
- a client for GraphQL introspection: see `caliban.tools.IntrospectionClient`.
- a way to compare GraphQL schemas, whether they come from Caliban or a remote server, see below.

## Dependency

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-tools" % "0.9.5"
```

## Schema comparison

The object `caliban.tools.SchemaComparison` exposes a `compare` function that compares 2 schemas from different origins. It takes 2 `SchemaLoader` as arguments, which you can build with one of the following constructors:
- `fromCaliban`: pass your `GraphQL` object from Caliban
- `fromFile`: pass the path to a file containing your schema in the GraphQL IDL
- `fromString`: pass a string containing your schema in the GraphQL IDL
- `fromIntrospection`: pass the URL of a GraphQL server supporting introspection

The output of `compare` is a `Task[List[SchemaComparisonChange]]`, with `SchemaComparisonChange` being a sealed trait representing the various kinds of changes. `SchemaComparisonChange#breaking` indicates if the change is breaking, such as removing a field or a type. `SchemaComparisonChange#toString` will return a nice description of the change.

The following example will compare the schema obtained by Caliban with a schema defined in a string and print the differences.

```scala
import caliban.GraphQL.graphQL
import caliban.RootResolver
import caliban.tools._
import zio.UIO

// schema from String
val schema: String =
  """
  type Hero {
    name(pad: Int!): String!
    nick: String!
    bday: Int
  }
  
  type Query {
    hero: Hero!
  }"""

// schema from Caliban
case class NameArgs(pad: Int)
case class Hero(name: NameArgs => String, nick: String, bday: Option[Int])
case class Query(hero: Hero)

val api = graphQL(RootResolver(Query(Hero(_ => "name", "nick", None))))

for {
  diff <- SchemaComparison.compare(SchemaLoader.fromString(schema), SchemaLoader.fromCaliban(api))
  _    <- UIO(println(diff.mkString("\n")))
} yield ()
```