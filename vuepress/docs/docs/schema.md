# Schemas

A GraphQL schema will be derived automatically at compile-time (no reflection) from the types present in your resolver.
The table below shows how common Scala types are converted to GraphQL types.

| Scala Type       | GraphQL Type                                     |
| ---------------- | ------------------------------------------------ |
| Boolean          | Boolean                                          |
| Int              | Int                                              |
| Float            | Float                                            |
| Double           | Float                                            |
| String           | String                                           |
| Unit             | Unit (custom scalar)                             |
| Long             | Long (custom scalar)                             |
| BigInt           | BigInt (custom scalar)                           |
| BigDecimal       | BigDecimal (custom scalar)                       |
| Case Class       | Object                                           |
| Sealed Trait     | Enum or Union                                    |
| Option[A]        | Nullable A                                       |
| List[A]          | List of A                                        |
| Set[A]           | List of A                                        |
| A => B           | A and B                                          |
| (A, B)           | Object with 2 fields `_1` and `_2`               |
| Either[A, B]     | Object with 2 nullable fields `left` and `right` |
| Map[A, B]        | List of Object with 2 fields `key` and `value`   |
| ZIO[R, E, A]     | Nullable A                                       |
| ZStream[R, E, A] | A                                                |

See the [Custom Types](#custom-types) section to find out how to support your own types.

If you want Caliban to support other standard types, feel free to [file an issue](https://github.com/ghostdogpr/caliban/issues) or even a PR.

::: warning
Magnolia (the library used to derive the schema at compile-time) sometimes has some trouble generating schemas with a lot of nested types, or types reused in multiple places.
to deal with this, you can declare schemas for your case classes and sealed traits explicitly:

```scala
implicit val roleSchema      = Schema.gen[Role]
implicit val characterSchema = Schema.gen[Character]
```

Make sure those implicits are in scope when you call `graphQL(...)`. This will make Magnolia's job easier by pre-generating schemas for those classes and re-using them when needed.
This will also improve compilation times and generate less bytecode.
:::

## Enum and union

A sealed trait will be converted to a different GraphQL type depending on its content:

- a sealed trait with only case objects will be converted to an `ENUM`
- a sealed trait with only case classes will be converted to a `UNION`

GraphQL does not support empty objects, so in case a sealed trait mixes case classes and case objects, a union type will be created and the case objects will have a "fake" field named `_` which is not queryable.

```scala
sealed trait ORIGIN
object ORIGIN {
  case object EARTH extends ORIGIN
  case object MARS  extends ORIGIN
  case object BELT  extends ORIGIN
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

## Arguments

To declare a field that take arguments, create a dedicated case class representing the arguments and make the field a _function_ from this class to the result type.

```scala
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

## Effects

Fields can return ZIO effects. This allows you to leverage all the features provided by ZIO: timeouts, retries, access to ZIO environment, memoizing, etc. An effect will be ran every time a query requiring the corresponding field is executed.

```scala
case class Queries(characters: Task[List[Character]],
                   character: CharacterName => RIO[Console, Character])
```

If you don't use ZIO environment (`R` = `Any`), there is nothing special to do to get it working.

If you require a ZIO environment, you will need to have the content of `caliban.schema.GenericSchema[R]` for your custom `R` in scope when you call `graphQL(...)`.

```scala
object schema extends GenericSchema[MyEnv]
import schema._
```

## Annotations

Caliban supports a few annotation to enrich data types:

- `@GQLName("name")` allows you to specify a different name for a data type or a field.
- `@GQLInputName("name")` allows you to specify a different name for a data type used as an input (by default, the suffix `Input` is appended to the type name).
- `@GQLDescription("description")` lets you provide a description for a data type or field. This description will be visible when your schema is introspected.
- `@GQLDeprecated("reason")` allows deprecating a field or an enum value.

## Custom types

Caliban provides auto-derivation for common types such as `Int`, `String`, `List`, `Option`, etc. but you can also support your own types by providing an implicit instance of `caliban.schema.Schema`.

An easy way to do this is to reuse existing instances and use `contramap` to map from your type to the original type. Here's an example of creating an instance for [refined](https://github.com/fthomas/refined)'s `NonEmptyString` reusing existing instance for `String`:

```scala
import caliban.schema._
implicit val nonEmptyStringSchema: Schema[NonEmptyString] = Schema.stringSchema.contramap(_.value)
```

You can also use the `scalarSchema` helper to create your own scalar types, providing a name, an optional description, and a function from your type to a `ResponseValue`:

```scala
import caliban.schema._
implicit val unitSchema: Schema[Unit] = scalarSchema("Unit", None, _ => ObjectValue(Nil))
```
