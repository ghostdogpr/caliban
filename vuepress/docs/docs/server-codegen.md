# Code generation

If you want a workflow where you first edit a GraphQL schema file, and then generate type-safe server stubs, Caliban has your back.

You'll first need to add the following dependency to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "2.7.2")
```

You then enable it in your `build.sbt` file:
```scala
import _root_.caliban.tools.Codegen

lazy val myproject = project
  // enable caliban codegen plugin
  .enablePlugins(CalibanPlugin)
  .settings(
    // add code generation settings
    Compile / caliban / calibanSettings ++= Seq(
      calibanSetting(file("myproject/src/main/graphql/myapi.graphql"))(
        // important to set this, otherwise you'll get client code
        _.genType(Codegen.GenType.Schema)
          // you can customize the codegen further with this DSL
          .clientName("NameOfApi.scala")
          .packageName("myproject.mypackage")
      ),
    )
  )
```

You can also generate it manually using the following sbt command:
```
calibanGenSchema schemaPath outputPath [options]
```

Example:
```
calibanGenSchema project/schema.graphql src/main/MyAPI.scala --addDerives true
```

## Options

- `scalafmtPath`: Specifies the configuration file for Scalafmt. Default: `.scalafmt.conf`.
- `headers`: Provides request headers when `schemaPath` is a URL.
- `packageName`: Overrides the package name derived from the folder of `outputPath`.
- `effect`: Overrides the default effect (`zio.UIO`) for wrapping fields in Queries and Mutations.
- `scalarMappings`: Forces a mapping between a GraphQL type and a Scala class (e.g., scalars).
- `imports`: Adds additional imports to the generated code.
- `abstractEffectType`: Indicates that the effect type is abstract. Fields in Queries and Mutations will return `F[_]`.
- `preserveInputNames`: Disables the default behavior of appending `Input` to the type name of input types in the derived schema.
- `addDerives`: Adds `derives` clauses for type class instance derivation in Scala 3.
- `envForDerives`: Specifies the type alias for your ZIO Environment when using `derives` and a ZIO Environment other than `Any`.

## Lazy evaluation

The main difference between generating code for client usage and for server usage is that on the server you need to account for 
code which should only be evaluated if the client requests the field!

You can annotate this directly in the graphql schema by creating a `@lazy` directive.

```graphql
directive @lazy on FIELD_DEFINITION
```

You can then annotate fields in the graphql schema like this:
```graphql
directive @lazy on FIELD_DEFINITION

type MyType {
    myLazyField: String! @lazy
    myField: String!
}
```

And you'll get a case class which looks something like this:
```scala
case class MyType(myLazyField: zio.UIO[String], myField: String)
```

When implementing this, `myLazyField` will only be evaluated if the client requested it in the query

## Newtype declaration

The `@newtype` directive in caliban allows you to wrap your GraphQL fields into statically
typed IDs for backend. For clients, they can use the GraphQL as before and do not
have to adjust their typing, or optionally can generate stronger typed IDs using
the directive.

In the following example we want to encapsulate `id : ID` as FooId for better type safety, so
we use the `@newtype` directive on the `Query` and Foo object type.
On mutation, we are passing an optional field of String. To avoid mixing with other String
types in backend code we decided to create a `@newtype` of Bar instead.

```graphql
directive @newtype(name : String) on FIELD_DEFINITION | ARGUMENT_DEFINITION | INPUT_FIELD_DEFINITION

type Query {
  getFoo(id: ID! @newtype(name: "FooId")): Foo
}

type Mutation {
  updateFoo(foo: FooInput!): Foo
}

type Foo {
  id: ID! @newtype(name: "FooId")
  ...
}

input FooInput {
  maybeBar: String @newtype(name : "Bar")
  ...
}
```

With Scalar mapping for ID set to Int, it would give you the following case classes.
```scala
case class Bar(value: String) extends AnyVal
case class FooId(value: Int)  extends AnyVal
```

The `extends AnyVal` ensures type erasure in such a way that we do not have to consider this abstraction
on client side, and hence graphql query remains the same as before applying the directive.
But for this to work we need to supply some implicit for our schema to understand the AnyVal
conversion and mapping of the return values using `@GQLDirective` annotation.

With this in place our generate type code should look something like this:

```scala
object Types {
  final case class QueryGetFooArgs(id: FooId)
  final case class MutationUpdateFooArgs(foo: FooInput)
  
  case class Bar(value: String) extends AnyVal
  object Bar   {
     implicit val schema: Schema[Any, Bar]    = implicitly[Schema[Any, String]].contramap(_.value)
     implicit val argBuilder: ArgBuilder[Bar] = implicitly[ArgBuilder[String]].map(Bar(_))
  }
  case class FooId(value: Int) extends AnyVal
  object FooId {
    implicit val schema: Schema[Any, FooId]    = implicitly[Schema[Any, Int]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[FooId] = implicitly[ArgBuilder[Int]].map(FooId(_))
  }
  final case class Foo(
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("FooId"))))
    id: FooId
    ...
  )
  final case class FooInput(
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("Bar"))))
    maybeBar: scala.Option[Bar]
    ...
  )
}
```
