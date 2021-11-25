# Http Adapters

Once you have an interpreter able to execute GraphQL queries, you usually want to expose this using an HTTP API.
Caliban comes with a few "ready-to-use" components (called "adapters") to expose your API with the most popular HTTP libraries. 
You can also create a custom adapter easily.

## Built-in adapters
Each built-in adapter comes with 3 main functions:
- `makeHttpService` takes an interpreter and turns it into a route following the [standard GraphQL protocol](https://graphql.org/learn/serving-over-http/#http-methods-headers-and-body)
- `makeHttpUploadService` takes an interpreter and turns it into a route following the [GraphQL multipart request protocol](https://github.com/jaydenseric/graphql-multipart-request-spec)
- `makeWebSocketService` takes an interpreter and turns it into a route following the [GraphQL WebSocket protocol](https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md)

Each of these functions also support a few parameters:
- `skipValidation` (default: false) allows skipping validation when executing requests
- `enableIntrospection` (default: true) allows disabling introspection
- `queryExecution` (default: Parallel) defines how query optimization should work
- `requestInterceptor` (default: empty) allows intercepting data from the request (e.g. headers) and do something with it (e.g. save it inside ZIO env)
- `webSocketHooks` (default: empty) gives you some hooks around the WebSocket lifecycle (useful for authentication)

The following adapters are provided:
- `Http4sAdapter` exposes a route for http4s, using circe for the Json handling.
- `ZHttpAdapter` exposes a route for zio-http, using circe for the Json handling. This one doesn't support upload yet.
- `PlayHttpAdapter` exposes a route for play, using play-json for the Json handling.
- `AkkaHttpAdapter` exposes a route for akka. For historical reasons, this adapter is not fixed to any Json library, which needs that you need depend on one of the tapir Json libraries and import it.

Want to use something else? Want to use one of them with a different Json library? Check the next section!

Make sure to check the many [examples](examples.md) as well.

## Make you own adapter