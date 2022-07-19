# Code Generation

Caliban provides two sbt plugins to generate your client(s) code.

The first one, named `CalibanPlugin`, allows you to generate the client code from a schema file or from a server URL, manually or automatically.

The second one, named `CompileTimeCalibanPlugin`, allows you to generate the client code from your server code.
This second "meta" plugin is actually made of two "concrete" plugins, `CompileTimeCalibanServerPlugin` and `CompileTimeCalibanClientPlugin`, that you'll
both need to configure in your project to be able to generate you Caliban client code from your Caliban server code.

To use any of these two plugins, you'll first need to add following dependency to your `project/plugins.sbt` file:
```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "1.4.2")
```

## CalibanPlugin

The first step for building GraphQL queries with `caliban-client` is to generate boilerplate code from a GraphQL schema. For that, you need a file containing your schema (if your backend uses `caliban`, you can get it by calling `GraphQL#render` on your API).

And enable it in your `build.sbt` file:
```scala
enablePlugins(CalibanPlugin)
```

### From a schema file

At this point, the `caliban` command will cause any files in `src/main/graphql` to be translated into a Caliban-generated client library. This happens automatically any time you `compile`.

By default, all clients are generated with the same client name as the source file, in the `caliban` top-level package.

In order to supply more configuration options to the code generator, you can use the `calibanSettings` sbt setting, combined with the `calibanSetting` function to scope the settings to a particular file:
```scala
      // The `file("Service.graphql")` is a path suffix for some file in `src/main/graphql`
      Compile / caliban / calibanSettings += calibanSetting(file("Service.graphql"))(
        cs =>
          cs.packageName("com.example.graphql.client")
            .scalarMapping(
              "LanguageCode" -> "com.example.models.LanguageCode",
            )
            .scalarMapping(
              "Timestamp" -> "java.sql.Timestamp",
              "DayOfWeek" -> "java.time.DayOfWeek",
              "IntRange"  -> "com.github.tminglei.slickpg.Range[Int]"
            )
            .imports("com.example.graphql.client.implicits._")
      )
```

The path where the generator will look for schemas can be customized by overriding the `calibanSources` settings:

```scala
Compile / caliban / calibanSources := file("caliban")
```

If you want to cherry-pick certain files yourself, you can override that as well with an explicit `caliban / sources` entry:

```scala
Compile / caliban / sources := List(file("caliban") / "Service.graphql")
```

For every entry in `calibanSettings` for the same file, a separate client (or [schema](schema.md#code-generation), depending on the entry's `genType` value) will be generated.

### From a server URL

The `calibanSetting` function also permits generating clients for supplied `url`'s:
```scala
      Compile / caliban / calibanSettings += calibanSetting(url("http://my-example-service/graphql"))(
        cs =>
          cs.clientName("ExampleServiceClient")
            .packageName("com.example.graphql.client")
      )
```

### Generation settings

The settings available on the `cs` (`CalibanSettings`) builder are:
- `packageName`: The package in which the code will be generated (default: `caliban`).
- `scalafmtPath`: Path to a scalafmt config file (default: `.scalafmt.conf`).
- `genView`: If true, will generate a case class and helper method to select all fields on an object (default: `false`).
- `scalarMappings`: A mapping from GraphQL scalar types to JVM types, as unknown scalar types are represented as `String` by default.
- `imports`: A list of imports to be added at the top of the generated code.
- `splitFiles`: Whether to split the generated code into multiple files (default: `false`).
- `enableFmt`: Enable code formatting with scalafmt (default: `true`).
- `extensibleEnums`: Generate a fallback case class for unknown enum values (default: `false`).
- `headers` (only defined for `url` settings): Supply extra headers when fetching the schema from a URL.

### Manual generation

If you prefer to generate the client explicitly rather than automatically, you can use `calibanGenClient` on the SBT CLI as follows:

```bash
calibanGenClient schemaPath outputPath [--scalafmtPath path] [--headers name:value,name2:value2] [--genView true|false] [--scalarMappings gqlType:f.q.d.n.Type,gqlType2:f.q.d.n.Type2] [--imports a.b.c._,c.d.E] [--splitFiles true|false] [--enableFmt true|false]

# example
calibanGenClient project/schema.graphql src/main/client/Client.scala --genView true  
```
This command will generate a Scala file in `outputPath` containing helper functions for all the types defined in the provided GraphQL schema defined at `schemaPath`.

If you need to disable generating clients from `src/main/graphql`, include `Compile / caliban / calibanGenerator := Seq.empty` in your project settings.

The package of the generated code is derived from the folder of `outputPath`. This can be overridden by providing an alternative package with the `--packageName` option. 
Similarly, the generated object name is derived from `outputPath` file name but can be overridden with the `--clientName` option.

Other options are explained above.

## CompileTimeCalibanPlugin

As mentioned in the introduction of the [Code Generation](#code-generation) chapter, this "meta" plugin is actually made of two "concrete" sbt plugins, `CompileTimeCalibanServerPlugin` and `CompileTimeCalibanClientPlugin`, 
that you'll both need to configure in your project be able to generate your Caliban client code from your Caliban server code..

You can find a demo project using this plugin here: [Demo project](https://github.com/guizmaii/poc_compile_time_caliban_client_generation)

To generate the Caliban client code from you Caliban server code, you need to do two things:
 1. Tell to the plugin where your Caliban `GraphQL[R]` instances for which you want to generate a client are and configure the client code generator.    
    How to configure this is explained in the following [Server side configuration](#server-side-configuration) chapter.
 
 2. Tell to the plugin where you want to generate your client(s).   
    How to configure this is explained in the following [Client side configuration](#client-side-configuration) chapter.

### Server side configuration

First, you'll need to activate the `CompileTimeCalibanServerPlugin` plugin in all the sbt modules of your project containing a `GraphQL[R]` instance for which you want to generate a client.

Let's say you have an `api` sbt module defined in your `build.sbt` which contains your Caliban server code:
```scala
lazy val api =
  project
    .enablePlugins(CompileTimeCalibanServerPlugin)
```

Now, you need to tell to the "server side" plugin where is your `GraphQL[R]` instance for which you want to generate a client.    
This `GraphQL[R]` instance need to be `public`. If it's `private` or `protected`, the plugin code generator will not have access to it and will fail.

Let's say you have an object `CalibanServer` object in your `api` sbt module:
```scala
package com.example.my.awesome.project.api

import caliban.GraphQL.graphQL
import caliban.GraphQL

object CalibanServer {
  
  val graphqlApi: GraphQL[MyEnv] = graphQL(Resolvers.resolver)
  
}
```

You'll need to add in your sbt definition:
```scala
lazy val api =
  project
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanServerSettings :=
        Seq(
          "com.example.my.awesome.project.api.CalibanServer.graphqlApi" -> ClientGenerationSettings.default
        )
    )
```

This is the minimal working configuration for the "server side".

Now, you may want to tweak how the client code is generated.    
For that, you'll have to replace the `ClientGenerationSettings.default` with the configuration that suits you the best.    
This `ClientGenerationSettings` case class gives you the following configuration options:

 - `packageName`: The package in which the code will be generated (default: `generated`).
 - `clientName`: The name of the client class generated (default: `Client`).
 - `scalafmtPath`: Path to a scalafmt config file (default: `.scalafmt.conf`).
 - `genView`: If true, will generate a case class and helper method to select all fields on an object (default: `false`).
 - `scalarMappings`: A mapping from GraphQL scalar types to JVM types, as unknown scalar types are represented as `String` by default.
 - `imports`: A list of imports to be added at the top of the generated code.
 - `splitFiles`: Whether to split the generated code into multiple files (default: `false`).
 - `enableFmt`: Enable code formatting with scalafmt (default: `true`).
 - `extensibleEnums`: Generate a fallback case class for unknown enum values (default: `false`).

Let's take an example:
```scala
lazy val api =
  project
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanServerSettings :=
        Seq(
          "com.example.my.awesome.project.api.CalibanServer.graphqlApi" ->
            ClientGenerationSettings(
              packageName = "com.example.my.awesome.project.client.generated",
              clientName = "CalibanClient",
              splitFiles = true
            ),
        )
    )
```

That's all. You now know how to configure the "server side" of this plugin.    
Let's now see how to configure the "client side".

### Client side configuration

The "client side" of this plugin is here to help you define where your Caliban client code is generated.

The first thing to do is to activate the `CompileTimeCalibanClientPlugin` in the sbt module where you want your Caliban client code to be generated into.

Let's say you have a `client` sbt module defined in your `build.sbt`:
```scala
lazy val client =
  project
    .enablePlugins(CompileTimeCalibanClientPlugin)
```

You only have one thing left to do.     
You need to reference your "server side" sbt module (here `api`) in your "client side" sbt module (here `client`) definition so the plugin knows that you want to generate the Caliban client code for your `api` server 
in this `client` sbt module:
```scala
lazy val client =
  project
    .enablePlugins(CompileTimeCalibanClientPlugin)
    .settings(
      Compile / ctCalibanClient / ctCalibanClientsSettings := Seq(api)
    )
```
This is the minimal working configuration for the "client side".

By default, the Caliban client code will be generated in your `src/main/scala` directory of your `client` sbt module.    
You may prefer it not to be generated inside your - usually versioned-in-git - module code. 
For that, the plugin provides an option to generate the code in the `target` directory instead:
```scala
lazy val client =
  project
    .enablePlugins(CompileTimeCalibanClientPlugin)
    .settings(
      Compile / ctCalibanClient / ctCalibanClientsSettings := Seq(api),
      Compile / ctCalibanClient / ctCalibanClientsVersionedCode := false // By default, it's true.
    )
```

You're done. :tada:    
You can now reload your sbt config and recompile your project. Your Caliban client code will be generated during the compilation process.

### Additional information about CompileTimeCalibanPlugin

As you may have seen in the [demo project](https://github.com/guizmaii/poc_compile_time_caliban_client_generation), you can have more complex configurations for this plugin.    
You can have more than one `GraphQL[R]` instance per server. Each `GraphQL[R]` instance can have its own client code generation configuration.      
You can also have multiple "servers" referenced in your "client" module. The plugin will generate all the clients for all the "servers" referenced in your sbt definition.
