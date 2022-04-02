# Schemas

A GraphQL schema will be derived automatically at compile-time (no reflection) from the types present in your resolver.
The table below shows how common Scala types are converted to GraphQL types.

| Scala Type                                          | GraphQL Type                                                     |
| --------------------------------------------------- | ---------------------------------------------------------------- |
| Boolean                                             | Boolean                                                          |
| Int                                                 | Int                                                              |
| Float                                               | Float                                                            |
| Double                                              | Float                                                            |
| String                                              | String                                                           |
| java.util.UUID                                      | ID                                                               |
| Unit                                                | Unit (custom scalar)                                             |
| Long                                                | Long (custom scalar)                                             |
| BigInt                                              | BigInt (custom scalar)                                           |
| BigDecimal                                          | BigDecimal (custom scalar)                                       |
| java.time.Instant                                   | Instant (custom scalar)                                          |
| java.time.LocalDate                                 | LocalDate (custom scalar)                                        |
| java.time.LocalTime                                 | LocalTime (custom scalar)                                        |
| java.time.LocalDateTime                             | LocalDateTime (custom scalar)                                    |
| java.time.OffsetDateTime                            | OffsetDateTime (custom scalar)                                   |
| java.time.ZonedDateTime                             | ZonedDateTime (custom scalar)                                    |
| Case Class                                          | Object                                                           |
| Sealed Trait                                        | Enum or Union                                                    |
| Option[A]                                           | Nullable A                                                       |
| List[A]                                             | List of A                                                        |
| Set[A]                                              | List of A                                                        |
| Seq[A]                                              | List of A                                                        |
| Vector[A]                                           | List of A                                                        |
| A => B                                              | A and B                                                          |
| (A, B)                                              | Object with 2 fields `_1` and `_2`                               |
| Either[A, B]                                        | Object with 2 nullable fields `left` and `right`                 |
| Map[A, B]                                           | List of Object with 2 fields `key` and `value`                   |
| ZIO[R, Nothing, A]                                  | A                                                                |
| ZIO[R, Throwable, A]                                | Nullable A                                                       |
| Future[A]                                           | Nullable A                                                       |
| ZStream[R, Throwable, A]                            | A (subscription) or List of A (query, mutation)                  |
| Json (from [Circe](https://github.com/circe/circe)) | Json (custom scalar, need `import caliban.interop.circe.json._`) |
| Json (from [play-json](https://github.com/playframework/play-json)) | Json (custom scalar, need `import caliban.interop.play.json._`) |

See the [Custom Types](#custom-types) section to find out how to support your own types.

If you want Caliban to support other standard types, feel free to [file an issue](https://github.com/ghostdogpr/caliban/issues) or even a PR.

::: warning Schema derivation issues
The schema derivation sometimes has some trouble generating schemas with a lot of nested types, or types reused in multiple places.
To deal with this, you can declare schemas for your case classes and sealed traits explicitly:

```scala
implicit val roleSchema      = Schema.gen[Any, Role]
implicit val characterSchema = Schema.gen[Any, Character]
```

Make sure those implicits are in scope when you call `graphQL(...)`. This will make derivation's job easier by pre-generating schemas for those classes and re-using them when needed.
This will also improve compilation times and generate less bytecode.

In Scala 2, if the derivation fails and you're not sure why, you can also call Magnolia's macro directly by using `genMacro`.
The compilation will return better error messages in case something is missing:

```scala
implicit val characterSchema = Schema.genMacro[Character].schema
```

In Scala 3, derivation doesn't support value classes and opaque types. You can use `Schema.genDebug` to print the generated code in the console.

:::

## Enums, unions, interfaces

A sealed trait will be converted to a different GraphQL type depending on its content:

- a sealed trait with only case objects will be converted to an `ENUM`
- a sealed trait with only case classes will be converted to a `UNION`

GraphQL does not support empty objects, so in case a sealed trait mixes case classes and case objects, a union type will be created and the case objects will have a "fake" field named `_` which is not queryable.

```scala mdoc:silent
sealed trait Origin
object Origin {
  case object EARTH extends Origin
  case object MARS  extends Origin
  case object BELT  extends Origin
}
```

The snippet above will produce the following GraphQL type:

```graphql
enum Origin {
  BELT
  EARTH
  MARS
}
```

Here's an example of union:

```scala
sealed trait Role
object Role {
  case class Captain(shipName: String) extends Role
  case class Engineer(specialty: String) extends Role
  case object Mechanic extends Role
}
```

The snippet above will produce the following GraphQL type:

```graphql
union Role = Captain | Engineer | Mechanic

type Captain {
  shipName: String!
}

type Engineer {
  specialty: String!
}

type Mechanic {
  _: Boolean!
}
```

If your type needs to be shared between multiple unions you can use the `@GQLValueType` annotation to have caliban
proxy to another type beyond the sealed trait.

```scala
case class Pilot(callSign: String)

sealed trait Role
object Role {
  case class Captain(shipName: String) extends Role
  case class Engineer(specialty: String) extends Role
  
  @GQLValueType
  case class Proxy(pilot: Pilot)
}
```

This will produce the following GraphQL Types:

```graphql
union Role = Captain | Engineer | Pilot

type Captain {
  shipName: String!
}

type Engineer {
  specialty: String!
}

type Pilot {
  callSign: String!
}
```

If you prefer an `Interface` instead of a `Union` type, add the `@GQLInterface` annotation to your sealed trait.
An interface will be created with all the fields that are common to the case classes extending the sealed trait, as long as they return the same type.

If you prefer to have a `Union` type instead of an `Enum`, even when the sealed trait contains only objects, add the `@GQLUnion` annotation.

## Arguments

To declare a field that take arguments, create a dedicated case class representing the arguments and make the field a _function_ from this class to the result type.

```scala mdoc:silent
case class Character(name: String, origin: Origin)
case class FilterArgs(origin: Option[Origin])
case class Queries(characters: FilterArgs => List[Character])
```

The snippet above will produce the following GraphQL type:

```graphql
type Queries {
  characters(origin: Origin): [Character!]!
}
```

Caliban provides auto-derivation for common types such as `Int`, `String`, `List`, `Option`, etc. but you can also support your own types by providing an implicit instance of `caliban.schema.ArgBuilder`.

::: tip
There is no `ArgBuilder` for tuples. If you have multiple arguments, use a case class containing all of them instead of a tuple.
:::

## Effects

Fields can return ZIO effects. This allows you to leverage all the features provided by ZIO: timeouts, retries, access to ZIO environment, memoizing, etc. An effect will be run every time a query requiring the corresponding field is executed.

```scala mdoc:silent:nest
import zio._

type CharacterName = String
case class Queries(characters: Task[List[Character]],
                   character: CharacterName => RIO[Console, Character])
```

If you don't use ZIO environment (`R` = `Any`), there is nothing special to do to get it working.

If you require a ZIO environment and use Scala 2, you will need to have the content of `caliban.schema.GenericSchema[R]` for your custom `R` in scope when you call `graphQL(...)`.
When you call `Schema.gen`, make sure to use your environment as the first type parameter.
```scala mdoc:silent
import caliban._
import caliban.schema._

type MyEnv = Console 

object schema extends GenericSchema[MyEnv]
import schema._

implicit val queriesSchema: Schema[MyEnv, Queries] = Schema.gen
// or
// implicit val queriesSchema = Schema.gen[MyEnv, Queries]
```

If you require a ZIO environment and use Scala 3, things are simpler since you don't need `GenericSchema`. Make sure to use `Schema.gen` with the proper R type parameter.
To make sure Caliban uses the proper environment, you need to specify it explicitly to `graphQL(...)`, unless you already have `Schema` instances for your root operations in scope.
```scala mdoc:silent
val queries = Queries(Task(???), _ => RIO(???))
val api = GraphQL.graphQL[MyEnv, Queries, Unit, Unit](RootResolver(queries))
// or
// implicit val queriesSchema: Schema[MyEnv, Queries] = Schema.gen
// val api = graphQL(RootResolver(queries)) // it will infer MyEnv thanks to the instance above
```

## Annotations

Caliban supports a few annotations to enrich data types:

- `@GQLName("name")` allows you to specify a different name for a data type or a field.
- `@GQLInputName("name")` allows you to specify a different name for a data type used as an input (by default, the suffix `Input` is appended to the type name).
- `@GQLDescription("description")` lets you provide a description for a data type or field. This description will be visible when your schema is introspected.
- `@GQLDeprecated("reason")` allows deprecating a field or an enum value.
- `@GQLExcluded` allows you to hide a field from the generated schema.
- `@GQLInterface` to force a sealed trait generating an interface instead of a union.
- `@GQLDirective(directive: Directive)` to add a directive to a field or type.
- `@GQLValueType(isScalar)` forces a type to behave as a value type for derivation. Meaning that caliban will ignore the outer type and take the first case class parameter as the real type. If `isScalar` is true, it will generate a scalar named after the case class (default: false).
- `@GQLDefault("defaultValue")` allows you to specify a default value for an input field using GraphQL syntax. The default value will be visible in your schema's SDL and during introspection.

## Java 8 Time types

Caliban provides implicit `Schema` types for the standard `java.time` types, by default these will use the
ISO standard strings for serialization and deserialization. However, you can customize this behavior by using
explicit constructor available under the `ArgBuilder` companion object. For instance, you can specify an `instantEpoch` 
to handle instants which are encoded using a `Long` from the standard java epoch time (January 1st 1970 00:00:00).
For some time formats you can also specify a specific `DateTimeFormatter` to handle your particular date time needs. 

## Custom types

Caliban provides auto-derivation for common types such as `Int`, `String`, `List`, `Option`, etc. but you can also support your own types by providing an implicit instance of `caliban.schema.Schema`.

An easy way to do this is to reuse existing instances and use `contramap` to map from your type to the original type. Here's an example of creating an instance for [refined](https://github.com/fthomas/refined)'s `NonEmptyString` reusing existing instance for `String` (if you use `refined`, you might want to look at [caliban-refined](https://github.com/niqdev/caliban-extras#caliban-refined)):

```scala
import caliban.schema._
implicit val nonEmptyStringSchema: Schema[Any, NonEmptyString] = Schema.stringSchema.contramap(_.value)
```

You can also use the `scalarSchema` helper to create your own scalar types, providing a name, an optional description, and a function from your type to a `ResponseValue`:

```scala mdoc:silent:reset
import caliban.schema._
import caliban.ResponseValue.ObjectValue

implicit val unitSchema: Schema[Any, Unit] = Schema.scalarSchema("Unit", None, None, _ => ObjectValue(Nil))
```

If you are using a custom type as part of the input you also have to provide an implicit instance of `caliban.schema.ArgBuilder`. For example here's how to do that for `java.time.LocalDate`:

```scala mdoc:silent
import java.time.LocalDate
import scala.util.Try

import caliban.Value
import caliban.CalibanError.ExecutionError
import caliban.schema.ArgBuilder

implicit val localDateArgBuilder: ArgBuilder[LocalDate] = {
  case Value.StringValue(value) =>
    Try(LocalDate.parse(value))
      .fold(ex => Left(ExecutionError(s"Can't parse $value into a LocalDate", innerThrowable = Some(ex))), Right(_))
  case other => Left(ExecutionError(s"Can't build a LocalDate from input $other"))
}
```

Value classes (`case class SomeWrapper(self: SomeType) extends AnyVal`) will be unwrapped by default in Scala 2 (this is not supported by Scala 3 derivation).

## Code generation

Caliban can automatically generate Scala code from a GraphQL schema.

In order to use this feature, add the `caliban-codegen-sbt` sbt plugin to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "1.4.0")
```

And enable it in your `build.sbt` file:
```scala
enablePlugins(CalibanPlugin)
```

Then call the `calibanGenSchema` sbt command.
```scala
calibanGenSchema schemaPath outputPath [--scalafmtPath path] [--headers name:value,name2:value2] [--packageName name] [--effect fqdn.Effect] [--scalarMappings gqlType:f.q.d.n.Type,gqlType2:f.q.d.n.Type2] [--imports a.b.c._,c.d.E] [--abstractEffectType true|false]

calibanGenSchema project/schema.graphql src/main/MyAPI.scala
```
This command will create a Scala file in `outputPath` containing all the types defined in the provided GraphQL schema defined at `schemaPath`. Instead of a file, you can provide a URL and the schema will be obtained using introspection.

The generated code will be formatted with Scalafmt using the configuration defined by `--scalafmtPath` option (default: `.scalafmt.conf`). If you provide a URL for `schemaPath`, you can provide request headers with `--headers` option.

The package of the generated code is derived from the folder of `outputPath`. This can be overridden by providing an alternative package with the `--packageName` option.

By default, each Query and Mutation will be wrapped into a `zio.UIO` effect. This can be overridden by providing an alternative effect with the `--effect` option.

You can also indicate that the effect type is abstract via `--abstractEffectType true`, in which case `Query` will be replaced by `Query[F[_]]` and so on (note `F` will be used unless `--effect <effect>` is explicitly given in which case `<effect>` would be used in place of `F`).

By default the suffix `Input` is appended to the type name of input types in the derived schema.  Use the `--preserveInputNames` flag to disable this. 

If you want to force a mapping between a GraphQL type and a Scala class (such as scalars), you can use the
`--scalarMappings` option. Also you can add additional imports by providing `--imports` option.

Since Caliban 1.3.0, you can generate schemas using an sbt `sourceGenerator`, which means your schemas will be generated every time you compile (or when you import your build into [Metals](https://scalameta.org/metals/)).
This can be configured with the same settings as [the client generators](client.md#code-generation), but you have to specify `.genType(Codegen.GenType.Schema)` in the `calibanSettings` entry for a given file.

## Building Schemas by hand

Sometimes for whatever reason schema generation fails. This can happen if your schema has co-recursive types and Magnolia is unable
to generate a schema for them. In cases like these you may need to instead create your own schema by hand.

Consider the case where you have three types which create cyclical dependencies on one another

```scala mdoc:silent
import java.util.UUID
import zio.UIO

case class Group(id: String, users: UIO[List[User]], parent: UIO[Option[Group]], organization: UIO[Organization])
case class Organization(id: String, groups: UIO[List[Group]])
case class User(id: String, group: UIO[Group])
```

These three types all depend on one another and if you attempt to generate a schema from them you will either end up with compiler errors or you will end up with a nasty runtime
error from a `NullPointerException`. To help the compiler out we can hand generate the types for these case classes instead.

```scala mdoc:silent
import caliban.schema.Schema
import caliban.schema.Schema.{obj, field}

implicit lazy val groupSchema: Schema[Any, Group] = obj("Group", Some("A group of users"))(
  implicit ft =>
    List(
      field("id")(_.id),
      field("users")(_.users),
      field("parent")(_.parent),
      field("organization")(_.organization)
    )
)
implicit lazy val orgSchema: Schema[Any, Organization] = obj("Organization", Some("An organization of groups"))(
  implicit ft =>
    List(
      field("id")(_.id),
      field("groups")(_.groups)
    )
)

implicit lazy val userSchema: Schema[Any, User] = obj("User", Some("A user of the service"))(
  implicit ft =>
    List(
      field("id")(_.id),
      field("group")(_.group)
    )
)
```
