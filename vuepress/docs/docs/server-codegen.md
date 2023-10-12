# Server code generation from schema

If you want a workflow where you first edit a graphql schema file, and then generate type-safe server stubs, Caliban has your back.

You'll first need to add the following dependency to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "2.3.1")
```

You then enable it in your `build.sbt` file:
```scala
import _root_.caliban.tools.Codegen

lazy val myproject = project
  // enable caliban codegen plugin
  .enablePlugins(CalibanPlugin)
  .settings(
    scalaVersion := "3.3.1",
    libraryDependencies ++= List(
      // the exact list of dependencies will vary with the libraries you want
      "com.github.ghostdogpr" %% "caliban" % "2.3.1",
      "com.github.ghostdogpr" %% "caliban-http4s" % "2.3.1",
      "com.github.ghostdogpr" %% "caliban-cats" % "2.3.1",
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "1.7.5",
      "org.http4s" %% "http4s-ember-server" % "0.23.19"
    ),
    Compile / caliban / calibanSettings ++= Seq(
      calibanSetting(file("myproject/src/main/graphql/myapi.graphql"))(
        // important to set this. otherwise you'll get client code
        _.genType(Codegen.GenType.Schema)
          // you can customize the codegen further with this DSL
          .clientName("NameOfApi.scala")
          .packageName("myproject.mypackage")
      ),
    )
  )
```

### Lazy evaluation

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

When implementing this, `myLazyField` will only be avaluated if the client requested it in the query

