# Schema Reporting

The `caliban-reporting` module allows you to integrate with Apollo's [schema reporting protocol](https://www.apollographql.com/docs/studio/schema/schema-reporting-protocol/).
This enables your servers to automatically publish updated schemas on start up without involving any additional tooling.

You can enable the settings by providing the `ReportingDaemon` to your `Runtime` during setup.

```scala mdoc:silent
import caliban.GraphQL
import caliban.reporting._
import sttp.client3.

val api: GraphQL[Any] = ??? // Define your GraphQL schema normally

// Define a SchemaReporter that will communicate with Apollo
val reporterL = SchemaReporter.fromDefaultConfig // Loads the access token from an environment variable called "APOLLO_KEY"
// Or load it from a configuration type
// val reporterL = SchemaReporter.fromConfig[ApolloConfig](_.key)

// Configure the daemon
val daemon = (for {
  ref <- SchemaReportingRef.make(api, "my-graph@production").toManaged_
  d   <- ReportingDaemon.make(ref)
} yield d).toLayer
```

Once defined you only need to add the daemon and the reporting layer to your system level wiring. The daemon
takes care of error handling with apollo and will dump a message to console if it encounters unrecoverable errors.