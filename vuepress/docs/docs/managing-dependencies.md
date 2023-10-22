# Managing Dependencies

Caliban provides integrations with many 3rd-party libraries.
In some cases, users might need to include additional modules from one of these integrations.

Let's use the `caliban-http4s` module as an example, where a user needs to include the server implementation:

```scala mdoc:silent
val calibanVersion  = "2.4.1"
val http4sVersion   = ??? // what version should we use?

libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban-http4s"       % calibanVersion,
  "org.http4s"            %% "http4s-ember-server"  % http4sVersion,
)
```

One sensible choice for this example would be to choose the latest `http4s` version. However, in some rare cases where
the versioning scheme is not strictly adhered to, this might lead to unexpected runtime errors if there is a mismatch
between the version of `http4s` that Caliban was build against for and the user-defined version.

## Caliban build-info SBT plugin

Starting with version 2.4.2, Caliban provides an SBT plugin that can be used to retrieve the versions of the libraries
that the current version of Caliban was built against. This makes it easier to manage additional dependencies from
libraries caliban integrates with.

To use the SBT plugin, add the following in the `project/plugins.sbt` file:

```scala
addSbtPlugin("com.github.ghostdogpr" % "caliban-build-info" % "2.4.2")
```

The dependency versions that caliban was build against can then be extracted in `build.sbt` as:

```scala
val calibanVersion  = _root_.caliban.BuildVersions.caliban
val http4sVersion   = _root_.caliban.BuildVersions.http4s

libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban-http4s"       % calibanVersion,
  "org.http4s"            %% "http4s-ember-server"  % http4sVersion
)
```
