# Http Adapters

Once you have an interpreter able to execute GraphQL queries, you usually want to expose it using an HTTP API.
Caliban comes with a few "ready-to-use" components (called "adapters") to expose your API with the most popular HTTP libraries.

Under the hood, adapters use the [tapir](https://tapir.softwaremill.com/en/latest/) library, so you can easily create a custom adapter with anything that tapir supports.

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

Make sure to check the [examples](examples.md) to see the adapters in action.

## Make you own adapter

All existing adapters are actually using a common adapter under the hood, called `TapirAdapter`.

This adapter, available in the `caliban-tapir` dependency, also have the same 3 methods `makeHttpService`, `makeHttpUploadService` and `makeWebSocketService`.
There are 2 main differences between these and the methods from the built-in adapters:
- they return tapir `ServerEndpoint` objects, which you can then pass to a tapir interpreter
- they require some implicit `JsonCodec`, which you can get by importing the proper tapir json object

Let's say we want to use `Http4sAdapter` but with play-json instead of circe. The built-in adapter uses circe so instead, we will directly use `TapirAdapter`.
First, we need to import 2 tapir dependencies in our project (in addition to `caliban-tapir`):
- a tapir interpreter for http4s: `tapir-zio-http4s-server`
- a tapir json codec for play-json: `tapir-json-play`

```scala
import sttp.tapir.json.play._
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter

val endpoints   = TapirAdapter.makeHttpService(interpreter)
val http4sRoute = ZHttp4sServerInterpreter().from(endpoints).toRoutes
```

That's it! `http4sRoute` is a valid http4s route ready to serve your API.

::: tip Limitations
The zio-http interpreter in tapir does not include multipart and websocket support.

Caliban comes with json encoders and decoders for circe, zio-json and play-json.
If you use another json library, you will need to create encoders and decoders for it (which is very simple, you can simply look at the existing ones).
:::
