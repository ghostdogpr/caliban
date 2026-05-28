# Getting Started

Caliban comes with a module called `caliban-tools` that exposes some useful features:
- a client for GraphQL introspection: see `caliban.tools.IntrospectionClient`.
- a way to [compare GraphQL schemas](schema-comparison.md), whether they come from Caliban or a remote server.

## Dependency

```scala
"com.github.ghostdogpr" %% "caliban-tools" % "3.1.2"
```

There is also another module called `caliban-codegen` that contains all the code generation features from `caliban-codegen-sbt`, so that you can use them without sbt: see `caliban.codegen.Codegen`.

## Dependency

```scala
"com.github.ghostdogpr" %% "caliban-codegen" % "3.1.2"
```