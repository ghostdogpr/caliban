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
- `Http4sAdapter` exposes a route for http4s.
- `ZHttpAdapter` exposes a route for zio-http. This one doesn't support uploads yet.
- `PlayHttpAdapter` exposes a route for play.
- `AkkaHttpAdapter` exposes a route for akka.

Want to use something else? Check [make your own adapter section](#make-your-own-adapter)!

Make sure to check the [examples](examples.md) to see the adapters in action.

## Json handling

Caliban comes with json encoders and decoders for circe, zio-json, jsoniter-scala and play-json.
Since v2.1.0, the adapters are not bound to a specific JSON handler and require the user to add the corresponding
dependency in their project and import the implicits in scope when calling the `makeHttpService` / `makeHttpUploadService` / `makeWebSocketService` methods.

- circe
  - `"com.softwaremill.sttp.tapir" %% "tapir-json-circe" % <version>`
  - `import sttp.tapir.json.circe._`
- jsoniter-scala
  - `"com.softwaremill.sttp.tapir" %% "tapir-jsoniter-scala" % <version>`
  - `import sttp.tapir.json.jsoniter._`
- play-json
  - `"com.softwaremill.sttp.tapir" %% "tapir-json-play" % <version>`
  - `import sttp.tapir.json.play._`
- zio-json
    - `"com.softwaremill.sttp.tapir" %% "tapir-json-zio" % <version>`
    - `import sttp.tapir.json.zio._`

Let's say we want to use `http4s` as the server implementation with `zio-json` as the json handler. Defining the http4s route is as simple as:

```scala
val http4sRoute = {
  import sttp.tapir.json.zio._
  Http4sAdapter.makeHttpService(interpreter)
}
```

That's it! `http4sRoute` is a valid http4s route ready to serve our API.

If you use another json library, you will need to create encoders and decoders for it (which is very simple, you can simply look at the existing ones).
The full list of JSON libraries supported by Tapir can be found [here](https://tapir.softwaremill.com/en/latest/endpoint/json.html)

:::tip Known issues (jsoniter-scala)
The `makeHttpUploadService` methods require an implicit of `JsonCodec[Map[String,Seq[String]]]` in scope. Jsoniter does not provide
codecs for common types by default, which means the user needs to create one. To do so, add the `jsoniter-scala-macros` dependency to your project and create one as:

```scala
import sttp.tapir.json.jsoniter._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

val http4sRoute = {
  import sttp.tapir.json.jsoniter._
  implicit val codec: JsonValueCodec[Map[String, Seq[String]]] = JsonCodecMaker.make

  Http4sAdapter.makeHttpUploadService(interpreter)
}
```

::: warning
To maximize performance, the **jsoniter** codec implementation is stack-recursive. To prevent stack overflow errors, it has a maximum depth limit of 512.

If your schema contains recursive types and want to use the jsoniter codecs, make sure to also limit the maximum query depth using
the [maxDepth wrapper](middleware.md#pre-defined-wrappers).
:::

## Make your own adapter

All existing adapters are actually using a common adapter under the hood, called `TapirAdapter`.

This adapter, available in the `caliban-tapir` dependency which has the same 3 methods `makeHttpService`, `makeHttpUploadService` and `makeWebSocketService`.

The main differences between these and the methods from the built-in adapters is that they return one or several tapir `ServerEndpoint`,
which you can then pass to a tapir interpreter. The returned `ServerEndpoint` use `RIO[R, *]` as an effect type, but you can easily transform it to another effect type. A helper `convertHttpEndpointToFuture` allows converting the effect type to a scala `Future` (this is used in the Akka and Play interpreters).
