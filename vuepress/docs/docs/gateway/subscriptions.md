# Subscriptions

The gateway supports subscriptions from local Caliban schemas and remote GraphQL services. A subscription can include fields fetched from other subgraphs. Each subscription root field must have one owner. Events arrive in source order.

## Transports

Use the existing Quick or Tapir adapters. Clients can use `graphql-transport-ws`, legacy WebSocket, or SSE with `Accept: text/event-stream`. Use POST for SSE. JSON and multipart HTTP responses cannot carry subscriptions.

Enable a downstream WebSocket route explicitly when serving with QuickAdapter:

```scala
QuickAdapter(interpreter).runServer(
  port = 4000,
  apiPath = "/graphql",
  graphiqlPath = Some("/graphiql"),
  webSocketPath = Some("/ws/graphql")
)
```

Clients connect to `ws://localhost:4000/ws/graphql`. SSE uses the HTTP `/graphql` route and needs no separate WebSocket route.

Remote subgraphs use `graphql-transport-ws` by default. They use the configured HTTP endpoint with the `ws` or `wss` scheme. Use `withEndpoint` on `RemoteSubscriptionConfig` to set a different subscription URL. To use SSE, configure the transport:

```scala
import caliban.gateway.{ RemoteGraphQLConfig, RemoteSubscriptionConfig }

val config = RemoteGraphQLConfig.default.withSubscription(
  _.withTransport(RemoteSubscriptionConfig.Sse())
)
```

Pass this config when you add the remote subgraph. `Sse(useGet = true)` selects GET instead of POST. For WebSocket authentication, use `withConnectionInit` to supply a static initialization payload. The usual remote header settings also apply.

Each remote subscription opens one connection. WebSocket acknowledgements, pong replies, and writes have a 30-second `connectionTimeout`. Pings use a 15-second `keepAliveInterval`. Configure client-facing keepalives through the adapter. Upstream legacy WebSocket and incremental `@defer` or `@stream` responses are unsupported.

## Consuming from Scala

```scala
import caliban.GraphQLRequest

val events = interpreter.executeStream(
  GraphQLRequest(query = Some("subscription { productChanged { name } }"))
)
```

Each consumption starts a new subscription. Cancelling it releases the connection and other resources. Provide authentication and scoped dependencies on the stream with `provideLayer`. To transform errors in individual events, use `events.map`. `interpreter.mapError` only covers setup.

## Limits and reconnecting

Configure limits with `GatewaySubscriptionConfig`:

```scala
val bounded = gateway.withConfig(_.withSubscriptions(_.withMaxActive(256).withBufferSize(16)))
```

Defaults are 1,024 active subscriptions and 32 buffered events each. `setupTimeout` allows 30 seconds to open a subscription. `eventTimeout` allows 30 seconds to process each event, regardless of the wait between events. Subscriptions have no lifetime limit. Remote messages are bounded by `RemoteGraphQLConfig.Execution.maxResponseBytes`. The ordinary request timeout does not end subscriptions.

- When the gateway reaches capacity, it rejects new subscriptions. A buffer overflow terminates the affected subscription with `SUBSCRIPTION_OVERFLOW` instead of dropping events.
- Remote messages exceeding `maxResponseBytes` terminate the subscription with `SUBSCRIPTION_EVENT_TOO_LARGE`.
- A schema reload ends existing subscriptions with the retryable `SUBSCRIPTION_SCHEMA_RELOAD` error. Clients must resubscribe. A failed reload leaves subscriptions running.
- The gateway does not reconnect upstream or replay events. Clients must handle terminal errors and reconnect. They may lose events while disconnected.

## Authentication and monitoring

Authenticate during setup. The gateway evaluates authorization once and captures forwarded headers for the subscription's lifetime. The WebSocket or authentication layer handles credential expiry and revocation. The gateway does not run authorization again for each event.

Remote error messages follow the gateway's `withRemoteErrorMessages` setting. The gateway retains only the `code` extension. Local sources keep Caliban's behavior. A field resolver failure can produce null without an error entry in that event.

The [metrics and tracing hooks](hooks.md#metrics-and-tracing) report subscription setup, events, and termination. Setup and event spans share the caller's trace when one is available.
