# FAQ

### I don't know where to start 😥

No worries! Head to the [Resources](https://ghostdogpr.github.io/caliban/resources/) page to find a few introductory videos and blog posts, in particular this [Beginner's Guide to GraphQL in Scala](https://blog.pierre-ricadat.com/a-beginners-guide-to-graphql-in-scala). Once you're ready for more details, check the [Documentation](https://ghostdogpr.github.io/caliban/docs/). If you prefer looking at some code first, check [this list of examples](https://ghostdogpr.github.io/caliban/docs/examples.html).

If you're still lost, just come to the [Discord channel](https://discord.gg/EYpumuv)!
 
### I'm getting a compilation error saying a `Schema` is missing, but I don't know which one.

Call directly `Schema.gen[YourType]` or just `gen[YourType]` if you extend `GenericSchema`. The error should be more detailed.
It is also recommended to use semi-auto derivation instead of auto derivation, as it will tell you more clearly which type is missing a schema.

### I want to use Caliban, but I use Cats Effect / Monix instead of ZIO.

That's not a problem! Caliban has interop modules that "hide" the ZIO details and expose Cats Effect or Monix types instead. Check the [interop docs](https://ghostdogpr.github.io/caliban/docs/interop.html#cats-effect) for more details.

### My query fails with an "Effect failure" error. How can I get more details?

When an error happens in one of the resolvers, Caliban doesn't expose the inner exception to the client by default (this might be unsafe to do so), but you can easily work around this by several ways:
1. If you fail with a `CalibanError.ExecutionError`, it won't be wrapped by Caliban so the original message will be displayed
2. Using the wrapper `@@ printErrors` will print the full error to the console
3. Using `mapError` on your interpreter lets you unwrap the `CalibanError` and return your inner exception instead

### I have more than 22 fields in my Query, I can't create a case class for it.

Instead of one huge case class with all your fields, you can create smaller case classes and combine `GraphQL` objects using `|+|`.

```scala
val api1 = graphQL(...)
val api2 = graphQL(...)

val api = api1 |+| api2
```

### How to deal with authentication/authorization?

This is typically handled with the help of ZIO environment. You can make your field require an `Auth` service by returning a `ZIO[Auth, E, A]`. Then, in your resolver, access the `Auth` service to check you have the appropriate permissions. You can inject the authentication information using a middleware in your HTTP server library. Check [here](https://github.com/search?q=repo%3Aghostdogpr%2Fcaliban+AuthExampleApp+language%3AScala&type=code&l=Scala) for a list of examples with different adapters.

### I have 2 case classes with the same name (different packages). How to avoid conflicts?
 
You can use the annotation `@GQLName` to give another name to any type or field.

### The auto-generated schema shows a field is nullable, but I want it non-nullable instead.

A GraphQL field is marked as nullable if the Scala field returns an `Option` or an effect that can fail. Change your effect to return `UIO` if you want the field to be non-nullable, and use `orDie` if you want to fail the whole query in case of error instead of returning null.

### Can I use a union as input?

Unfortunately, it is not supported by the GraphQL spec. See [https://github.com/graphql/graphql-spec/issues/488](https://github.com/graphql/graphql-spec/issues/488) for discussions. An alternative can be to define your own custom scalar.

### How to deal with recursive types?

Recursive types can be a little tricky. This is not a silver bullet but usually the trick is to add an `implicit lazy val` instance of `Schema` for the type that is recursive. See [here](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/optimizations/NaiveTest.scala#L82) for an example.

### I'm getting a "Method too large" compiler error.

When you create a GraphQL API and use auto schema derivation, Caliban generates a schema for every type, everywhere it's used. If you have a lot of types, the generated code might be too large. The workaround is to use semi-auto derivation instead and define a `Schema` for each of your case classes and sealed traits:
```scala
implicit val schemaMyType: Schema[Any, MyType] = Schema.gen
```
That way, the schema for this type will be extracted to a single method and defined only once.

### How can I define a `Schema` for a Java enum?

Here's an example for Java `DayOfWeek`:
```scala  
implicit val dayOfWeekSchema = new Schema[Any, DayOfWeek] {
  override def toType(isInput: Boolean): __Type =
    Types.makeEnum(
      Some("DayOfWeek"),
      None,
      DayOfWeek.values.toList.map(v => __EnumValue(v.name, None, false, None)),
      None
    )
  override def resolve(value: DayOfWeek): Step[Any] = PureStep(StringValue(value.name))
}
```

### I don't want to use `Throwable` as my error type

Caliban provides `Schema` instances for `ZIO`, `ZQuery` and `ZStream` but with the condition that the error type is a `Throwable`.
That is because the error is eventually wrapped inside Caliban `ExecutionError` and we need to know what it is. 
However, you can easily define a custom `Schema` without this constraint, as long as you provide a function from your error type to `ExecutionError`.

For example, if your error type is `Int`, you can use `Schema.customErrorEffectSchema` as follows:
```scala
implicit def customEffectSchema[A](implicit s: Schema[Any, A]): Schema[Any, IO[Int, A]] =
  Schema.customErrorEffectSchema((code: Int) => ExecutionError(s"Error code $code"))
```
With this implicit in scope, Caliban will know how to handle any `IO[Int, A]` effects.
Caliban will automatically fill the error path and the error location inside `ExecutionError` if an error happens during the query execution.

### My interface is missing from the schema

If you have an interface that is not directly returned by any field, it will be missing from the schema. This is a constraint from the way the typeclass derivation works. A workaround is to use `withAdditionalTypes` on your `GraphQL` object to explicitly add the interface. See the following example:
```scala mdoc:silent
import caliban._
import caliban.schema.Schema
import caliban.schema.Annotations._
import caliban.schema.Schema.auto._

@GQLInterface
sealed trait Interface

case class A(s: String) extends Interface
case class B(s: String) extends Interface

case class Query(a: A, b: B)

val interfaceType = Schema.gen[Any, Interface].toType_()

val api = graphQL(RootResolver(Query(A("a"), B("b")))).withAdditionalTypes(List(interfaceType))
```

### Can I check that a GraphQL query is valid at compile-time?

Caliban provides a little macro called `gqldoc` that can check at **compile-time** that a GraphQL query (a *document* to be exact) has valid syntax.

```scala mdoc:silent
import caliban.Macros.gqldoc

val query = gqldoc("""
  query test {
    amos: character(name: "Amos Burton") {
      name
    }
  }""")
```
