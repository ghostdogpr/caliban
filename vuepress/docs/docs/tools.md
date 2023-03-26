# Getting Started

Caliban comes with a module called `caliban-tools` that exposes some useful features:
- all the code generation features from `caliban-codegen-sbt`, so that you can use them without sbt: see `caliban.tools.Codegen`.
- a client for GraphQL introspection: see `caliban.tools.IntrospectionClient`.
- utilities for [stitching GraphQL schemas](stitching.md).
- a way to [compare GraphQL schemas](schema-comparison.md), whether they come from Caliban or a remote server.

## Dependency

```
libraryDependencies += "com.github.ghostdogpr" %% "caliban-tools" % "2.1.0"
```
