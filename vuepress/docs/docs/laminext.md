# Laminext Integration

If you are using the Scala.js framework [Laminar](https://laminar.dev), there is a module that makes the integration even nicer, with support for subscriptions.
It is depending on [Laminext](https://laminext.dev), a library that provides nice little helpers for Laminar, in particular for using `Fetch` and `WebSocket`.

To use it, import the `caliban-client-laminext` module:
```
libraryDependencies += "com.github.ghostdogpr" %%% "caliban-client-laminext" % "2.0.0"
```

Add the following import to your code:
```scala
import caliban.client.laminext._
```

This import adds an extension method `toEventStream(uri)` to `SelectionBuilder`, which is similar to `toRequest` except it creates an `EventStream` instead of an sttp `Request`.

```scala
val characters: Var[List[String]] = Var(Nil)

val uri = "http://localhost:8088/api/graphql"

val getCharacters = Queries.characters(None)(Client.Character.name).toEventStream(uri)

val view: Div = 
  div(
    "Characters: ",
    getCharacters.collectRight --> characters.set _,
    child <-- characters.signal.map(c => div(c.mkString(", ")))
  )
```

To use subscriptions, you first need to create a `WebSocket` with protocol `graphql-ws`. Use the extension method `.graphql` instead of `.text` or `.json`.
Then use the extension method `toSubscription` on your `SelectionBuilder` and pass the `WebSocket` object.
```scala
val ws = WebSocket.url("ws://localhost:8088/ws/graphql", "graphql-ws").graphql.build()

val deletedCharacters = Subscriptions.characterDeleted.toSubscription(ws)
```

Finally, you can use `ws.connect` to connect the `WebSocket`, `ws.init()` to initialize the communication with the graphql server and `.received` to get an `EventStream` of the type returned by your subscription.
```scala
ws.connect,
ws.connected --> (_ => ws.init()),
deletedCharacters.received.collectRight --> 
  (name => characters.update(_.filterNot(_ == name))),
```

There is a full example in the `test` folder of the `caliban-client-laminext` module.
To use it:
- run `ExampleApp` of the http4s server example (it supports CORS)
- run `clientLaminextJS/Test/fastLinkJS` to compile the Scala.js code
- run `yarn install` and `yarn exec vite` in the `caliban-client-laminext` folder
- the example page will be running on [http://localhost:3000](http://localhost:3000)
