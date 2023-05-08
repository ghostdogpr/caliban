# Schemas

A GraphQL schema will be derived automatically at compile-time (no reflection) from the types present in your resolver.
The table below shows how common Scala types are converted to GraphQL types.

| Scala Type                                                          | GraphQL Type                                                     |
|---------------------------------------------------------------------|------------------------------------------------------------------|
| Boolean                                                             | Boolean                                                          |
| Int                                                                 | Int                                                              |
| Float                                                               | Float                                                            |
| Double                                                              | Float                                                            |
| String                                                              | String                                                           |
| java.util.UUID                                                      | ID                                                               |
| Unit                                                                | Unit (custom scalar)                                             |
| Long                                                                | Long (custom scalar)                                             |
| BigInt                                                              | BigInt (custom scalar)                                           |
| BigDecimal                                                          | BigDecimal (custom scalar)                                       |
| java.time.Instant                                                   | Instant (custom scalar)                                          |
| java.time.LocalDate                                                 | LocalDate (custom scalar)                                        |
| java.time.LocalTime                                                 | LocalTime (custom scalar)                                        |
| java.time.LocalDateTime                                             | LocalDateTime (custom scalar)                                    |
| java.time.OffsetDateTime                                            | OffsetDateTime (custom scalar)                                   |
| java.time.ZonedDateTime                                             | ZonedDateTime (custom scalar)                                    |
| Case Class                                                          | Object                                                           |
| Sealed Trait                                                        | Enum or Union                                                    |
| Option[A]                                                           | Nullable A                                                       |
| List[A]                                                             | List of A                                                        |
| Set[A]                                                              | List of A                                                        |
| Seq[A]                                                              | List of A                                                        |
| Vector[A]                                                           | List of A                                                        |
| A => B                                                              | A and B                                                          |
| (A, B)                                                              | Object with 2 fields `_1` and `_2`                               |
| Either[A, B]                                                        | Object with 2 nullable fields `left` and `right`                 |
| Map[A, B]                                                           | List of Object with 2 fields `key` and `value`                   |
| ZIO[R, Nothing, A]                                                  | A                                                                |
| ZIO[R, Throwable, A]                                                | Nullable A                                                       |
| Future[A]                                                           | Nullable A                                                       |
| ZStream[R, Throwable, A]                                            | A (subscription) or List of A (query, mutation)                  |
| Json (from [Circe](https://github.com/circe/circe))                 | Json (custom scalar, need `import caliban.interop.circe.json._`) |
| Json (from [play-json](https://github.com/playframework/play-json)) | Json (custom scalar, need `import caliban.interop.play.json._`)  |

See the [Custom Types](#custom-types) section to find out how to support your own types.

If you want Caliban to support other standard types, feel free to [file an issue](https://github.com/ghostdogpr/caliban/issues) or even a PR.

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

## Case classes and sealed traits

The transformation between Scala types and GraphQL types is handled by a typeclass named `Schema`. As mentioned earlier, Caliban provides instances of `Schema` for all basic Scala types, but inevitably you will need to support your own types, in particular case classes and sealed traits.

Caliban is able to generate instances of `Schema` for case classes and sealed traits. You have two choices for doing that: auto derivation and semi-auto derivation.

### Auto derivation
Auto derivation is achieved easily by adding the following import:
```scala mdoc:silent
import caliban.schema.Schema.auto._
```
Using this import, Caliban will generate `Schema` instances for all the case classes and sealed traits that are found inside your resolver.

::: warning Auto derivation limitations
Auto derivation is the easiest way to get started, but it has some drawbacks:
- If a type is referenced in several places inside your resolver, a `Schema` will be generated for each occurrence, which can lead to longer compilation times and a high amount of generated code (a sign of this is that the compiler will suggest increasing `-Xmax-inlines` in Scala 3).
- When a `Schema` is missing for a nested type inside your resolver, it can sometimes be difficult to find out which type is missing when using auto derivation, because the error message will mention the root type and not the nested one.
- The macro that generates the `Schema` instances sometimes gets confused when there are a lot of nested or recursive types, and can mistakenly generate a `Schema` for types that already have a `Schema` in scope. For this reason, semi-auto derivation is recommended for non-trivial schemas.
:::

### Semi-auto derivation
Semi-auto derivation is achieved as follows for each type that needs a `Schema` instance (`MyClass` in the example):

<code-group>
  <code-block title="Scala 2" active>

```scala mdoc:silent
import caliban.schema.Schema

case class MyClass(field: String)

implicit val schemaForMyClass: Schema[Any, MyClass] = Schema.gen
```
  </code-block>
  <code-block title="Scala 3">

```scala
import caliban.schema.Schema

case class MyClass(field: String) derives Schema.SemiAuto

// if you don't want to use the `derives` syntax, you can also use the following:
given Schema[Any, MyClass] = Schema.gen
```
  </code-block>
</code-group>

In Scala 3, derivation doesn't support value classes and opaque types. You can use `Schema.genDebug` to print the generated code in the console.

### Combining auto and semi-auto derivation

For some types such as enums, it might be desirable to use auto derivation to reduce boilerplate schema definitions:

<code-group>
  <code-block title="Scala 2" active>

```scala mdoc:silent:reset
import caliban.schema.Schema

sealed trait Origin
object Origin {
  case object EARTH extends Origin
  case object MARS  extends Origin
  case object BELT  extends Origin
}

implicit val schemaForMyClass: Schema[Any, Origin] = {
  import Schema.auto._
  Schema.gen
}
```
  </code-block>
  <code-block title="Scala 3 (Any schema)">

```scala
import caliban.schema.Schema

enum Origin derives Schema.Auto {
  case EARTH, MARS, BELT
}

// if you don't want to use the `derives` syntax, you can also use the following:
given Schema[Any, Origin] = Schema.Auto.derived
```
  </code-block>
  <code-block title="Scala 3 (Custom schema)">

```scala
import caliban.schema.Schema

trait MyEnv
object EnvSchema extends Schema.SchemaDerivation[MyEnv]

enum Origin derives EnvSchema.Auto {
  case EARTH, MARS, BELT
}

// if you don't want to use the `derives` syntax:
given Schema[MyEnv, Origin] = EnvSchema.Auto.derived
```
  </code-block>
</code-group>

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

Caliban provides auto-derivation for common types such as `Int`, `String`, `List`, `Option`, etc. but you can also support your own types by providing an implicit instance of `caliban.schema.ArgBuilder` that defines how incoming arguments from that types should be extracted. You also need a `Schema` for those types.

Derivation of `ArgBuilder` for case classes works similarly to `Schema` derivation. You can use auto derivation by adding the following import, or via the `derives` keyword (Scala 3 only):


<code-group>
  <code-block title="Scala 2" active>

```scala mdoc:silent
import caliban.schema.ArgBuilder.auto._
```
  </code-block>
  <code-block title="Scala 3">

```scala
import caliban.schema.{ArgBuilder, Schema}

case class FieldArg(value: String)
case class MyClass(field: FieldArg) derives Schema.Auto, ArgBuilder.GenAuto

// if you don't want to use the `derives` syntax, you can also use the following:
given ArgBuilder[MyClass]  = ArgBuilder.GenAuto.derived
given Schema[Any, MyClass] = Schema.Auto.derived
```
  </code-block>
</code-group>

Or you can use semi-auto derivation as follows:

<code-group>
  <code-block title="Scala 2" active>

```scala mdoc:silent:reset
import caliban.schema.{ArgBuilder, Schema}

case class MyClass(field: String)

implicit val argBuilderForMyClass: ArgBuilder[MyClass] = ArgBuilder.gen
implicit val schemaForMyClass: Schema[Any, MyClass]    = Schema.gen
```
  </code-block>
  <code-block title="Scala 3">

```scala
import caliban.schema.{ArgBuilder, Schema}

case class MyClass(field: String) derives Schema.SemiAuto, ArgBuilder

// if you don't want to use the `derives` syntax, you can also use the following:
given ArgBuilder[MyClass]  = ArgBuilder.gen
given Schema[Any, MyClass] = Schema.gen
```
  </code-block>
</code-group>

::: tip
There is no `ArgBuilder` for tuples. If you have multiple arguments, use a case class containing all of them instead of a tuple.
:::

## Custom types

Caliban provides auto-derivation for common types such as `Int`, `String`, `List`, `Option`, etc. but you can also support your own types by providing an implicit instance of `caliban.schema.Schema`. Note that you don't have to do this if your types are just case classes composed of common types.

An easy way to do this is to reuse existing instances and use `contramap` to map from your type to the original type. Here's an example of creating an instance for [refined](https://github.com/fthomas/refined)'s `NonEmptyString` reusing existing instance for `String` (if you use `refined`, you might want to look at [caliban-refined](https://github.com/niqdev/caliban-extras#caliban-refined)):

```scala
import caliban.schema._
implicit val nonEmptyStringSchema: Schema[Any, NonEmptyString] = Schema.stringSchema.contramap(_.value)
```

You can also use the `scalarSchema` helper to create your own scalar types, providing a name, an optional description, and a function from your type to a `ResponseValue`:

```scala mdoc:silent:reset
import caliban.schema._
import caliban.ResponseValue.ObjectValue

implicit val unitSchema: Schema[Any, Unit] = Schema.scalarSchema("Unit", None, None, None, _ => ObjectValue(Nil))
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

## Effects

Fields can return ZIO effects. This allows you to leverage all the features provided by ZIO: timeouts, retries, access to ZIO environment, memoizing, etc. An effect will be run every time a query requiring the corresponding field is executed.

```scala mdoc:silent:reset
import zio._

type CharacterName = String
case class Character(name: CharacterName)
case class Queries(characters: Task[List[Character]],
                   character: CharacterName => RIO[Console, Character])
```

If you don't use ZIO environment (`R` = `Any`), there is nothing special to do to get it working.

If you require a ZIO environment and use Scala 2, you can't use `Schema.gen` or the import we saw previously because they expect `R` to be `Any`. Instead, you need to make a new object that extends `caliban.schema.GenericSchema[R]` for your custom `R`. Then you can use `gen` or `auto` from that object to generate your schema.
```scala mdoc:silent
import caliban._
import caliban.schema._

type MyEnv = Console 

object customSchema extends GenericSchema[MyEnv]
import customSchema.auto._

// if you use semi-auto generation, use this instead:
// implicit val characterSchema: Schema[MyEnv, Character] = customSchema.gen
// implicit val queriesSchema: Schema[MyEnv, Queries] = customSchema.gen

val queries = Queries(ZIO.attempt(???), _ => ZIO.succeed(???))
val api = graphQL(RootResolver(queries))
```

If you require a ZIO environment and use Scala 3, things are simpler since you don't need `GenericSchema`. Just make sure to use `Schema.gen` with the proper R type parameter.
To make sure Caliban uses the proper environment, you need to specify it explicitly to `graphQL(...)`, unless you already have `Schema` instances for your root operations in scope.
```scala
val queries = Queries(ZIO.attempt(???), _ => ZIO.succeed(???))
val api = graphQL[MyEnv, Queries, Unit, Unit](RootResolver(queries))
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

## Code generation

Caliban can automatically generate Scala code from a GraphQL schema.

In order to use this feature, add the `caliban-codegen-sbt` sbt plugin to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "2.2.0")
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
