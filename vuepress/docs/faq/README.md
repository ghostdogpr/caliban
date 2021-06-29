# FAQ

### I don't know where to start 😥

No worries! Head to the [Resources](https://ghostdogpr.github.io/caliban/resources/) page to find a few introductory videos and blog posts. Once you're ready for more details, check the [Documentation](https://ghostdogpr.github.io/caliban/docs/). If you prefer looking at some code first, check [this list of examples](https://ghostdogpr.github.io/caliban/docs/examples.html).

If you're still lost, just come to the [Discord channel](https://discordapp.com/channels/629491597070827530/633200096393166868)!
 
### I'm getting a compilation error saying a `Schema` is missing, but I don't know which one.

Call directly `Schema.gen[YourType]` or just `gen[YourType]` if you extend `GenericSchema`. The error should be more detailed.

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

This is typically handled with the help of ZIO environment. You can make your field require an `Auth` service by returning a `ZIO[Auth, E, A]`. Then, in your resolver, access the `Auth` service to check you have the appropriate permissions. You can inject the authentication information using a middleware in your HTTP server library. Check [here](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/http4s/AuthExampleApp.scala) for a full example using http4s.

### I have 2 case classes with the same name (different packages). How to avoid conflicts?
 
You can use the annotation `@GQLName` to give another name to any type or field.

### The auto-generated schema shows a field is nullable, but I want it non-nullable instead.

A GraphQL field is marked as nullable if the Scala field returns an `Option` or an effect that can fail. Change your effect to return `UIO` if you want the field to be non-nullable, and use `orDie` if you want to fail the whole query in case of error instead of returning null.

### Can I use a union as input?

Unfortunately, it is not supported by the GraphQL spec. See [https://github.com/graphql/graphql-spec/issues/488](https://github.com/graphql/graphql-spec/issues/488) for discussions. An alternative can be to define your own custom scalar.

### How to deal with recursive types?

Recursive types can be a little tricky. This is not a silver bullet but usually the trick is to add an `implicit lazy val` instance of `Schema` for the type that is recursive. See [here](https://github.com/ghostdogpr/caliban/blob/master/examples/src/main/scala/example/optimizations/NaiveTest.scala#L82) for an example.

### I'm getting a "Method too large" compiler error.

When you create a GraphQL API, Caliban (using a macro powered by [Magnolia](https://github.com/propensive/magnolia)) generates a schema for every type, everywhere it's used. If you use a lot of types, the generated code might be too large. The workaround is to define schemas for your intermediate types:
```scala
implicit val schemaMyType: Schema[Any, MyType] = Schema.gen
```
That way, the schema for this type will be extracted to a single method and defined only once. Do it in priority with types that are re-used in a lot of places: this will reduce the amount of generated code and will speed up compilation time.

### How can I defined a Schema for a Java enum?

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
