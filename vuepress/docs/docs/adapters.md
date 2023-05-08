# Http Adapters

Once you have an interpreter able to execute GraphQL queries, you usually want to expose it using an HTTP API.
Caliban comes with a few "ready-to-use" components (called "adapters") to expose your API with the most popular HTTP libraries.

Under the hood, adapters use the [tapir](https://tapir.softwaremill.com/en/latest/) library, so you can easily create a custom adapter with anything that tapir supports.

## Built-in adapters
The following adapters are provided:
- `Http4sAdapter` exposes a route for http4s.
- `ZHttpAdapter` exposes a route for zio-http. This one doesn't support uploads yet.
- `PlayHttpAdapter` exposes a route for play.
- `AkkaHttpAdapter` exposes a route for akka.

To use them, you first need to transform your `GraphQLInterpreter` into a new type of interpreter that supports the protocol you want to use.
There are 3 of them:
- `HttpInterpreter` follows the [standard GraphQL protocol](https://graphql.org/learn/serving-over-http/#http-methods-headers-and-body)
- `HttpUploadInterpreter` follows the [GraphQL multipart request protocol](https://github.com/jaydenseric/graphql-multipart-request-spec)
- `WebSocketInterpreter` follows the [GraphQL WebSocket protocol](https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md)

These interpreters expose 2 powerful methods:
- `configure` takes a `Configurator[R]` which is an alias for `URIO[R & Scope, Unit]`.
It allows you configuring the interpreter by running an effect that will run for each request and that can modify the configuration of the running fiber. 
Built-in configurators such as `Configurator.setSkipValidation`, `Configurator.setEnableIntrospection` and `Configurator.setQueryExecution` let you dynamically change the configuration of the interpreter.
- `intercept` takes an `Interceptor[-R1, +R]` which is an alias for `ZLayer[R1 & ServerRequest, TapirResponse, R]`.
It is basically a more powerful version of `configure` that gives you access to the incoming request (`ServerRequest`) and lets you modify the environment of the interpreter.
A typical use case would be to extract an authentication token from the request and eliminate the authentication requirement from the environment if the token is valid. See an example [here](https://github.com/ghostdogpr/caliban/blob/series/2.x/examples/src/main/scala/example/akkahttp/AuthExampleApp.scala#L51).

In addition to that, the `WebSocketInterpreter` constructor comes with 2 optional parameters:
- `keepAliveInterval` (default: empty) defines the interval for the server to send keep alive messages to the client
- `webSocketHooks` (default: empty) gives you some hooks around the WebSocket lifecycle (useful for authentication)

Once your interpreter is correctly configured, you can use one of these 3 functions exposed by each built-in adapter:
- `makeHttpService` turns an `HttpInterpreter` into a route for the corresponding library
- `makeHttpUploadService` turns an `HttpUploadInterpreter` into a route for the corresponding library
- `makeWebSocketService` turns a `WebSocketInterpreter` into a route for the corresponding library

```scala
val graphQLInterpreter: GraphQLInterpreter[AuthToken, CalibanError] = ???
// turn our GraphQL interpreter into an HttpInterpreter
val noAuthInterpreter: HttpInterpreter[AuthToken, CalibanError] = HttpInterpreter(graphQLInterpreter)
// define authentication logic (from a ServerRequest, fail or build an AuthToken)
val auth: ZLayer[ServerRequest, TapirResponse, AuthToken] = ???
// pass our interceptor to eliminate the AuthToken requirement from the environment
val authInterpreter: HttpUploadInterpreter[Any, CalibanError] = httpInterpreter.intercept(auth)
// get our route for Akka Http
val route = AkkaHttpAdapter.default().makeHttpService(authInterpreter)
```

Want to use something else? Check [make your own adapter section](#make-your-own-adapter)!

Make sure to check the [examples](examples.md) to see the adapters in action.

## Json handling

Caliban comes with JSON encoders and decoders for the following libraries:

- circe
- jsoniter-scala (JDK 11+ only)
- play-json
- zio-json

Since v2.1.0, the adapters are not bound to a specific JSON handler and require the user to add the [corresponding dependency](README.md#dependencies) in their project and import the implicits in scope when calling the `makeHttpService` / `makeHttpUploadService` / `makeWebSocketService` methods.

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

:::tip Known issues: jsoniter-scala
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
To maximize performance, the **jsoniter** codec implementation is stack-recursive. To prevent stack overflow errors, it has a **maximum depth limit of 512**.

If your schema contains recursive types and want to use the jsoniter codecs, make sure to also limit the maximum query depth using
the [maxDepth wrapper](middleware.md#pre-defined-wrappers).
:::

## Make your own adapter

All existing adapters are actually using a common adapter under the hood, called `TapirAdapter`.

This adapter, available in the `caliban-tapir` dependency which has the same 3 methods `makeHttpService`, `makeHttpUploadService` and `makeWebSocketService`.

The main differences between these and the methods from the built-in adapters is that they return one or several tapir `ServerEndpoint`,
which you can then pass to a tapir interpreter. The returned `ServerEndpoint` use `RIO[R, *]` as an effect type, but you can easily transform it to another effect type. A helper `convertHttpEndpointToFuture` allows converting the effect type to a scala `Future` (this is used in the Akka and Play interpreters).
