package caliban.gateway.hive

import caliban.IncomingRequestHeaders
import caliban.gateway.PhaseHooks.Event
import caliban.gateway.{ OperationEvent, PhaseHandler, PhaseHooks }
import caliban.hive.HiveUsage
import caliban.hive.HiveUsage.{ ClientInfo, Operation }
import zio._

import java.util.concurrent.TimeUnit

/**
 * GraphQL Hive usage reporting for a Caliban gateway.
 *
 * Attach [[hooks]] with `Gateway.compose(...) @@ GatewayHive.hooks` and provide `HiveUsage.layer(config)`.
 */
object GatewayHive {

  /**
   * Phase hooks that report every operation the gateway prepared to the [[HiveUsage]] in the environment, with the
   * coordinates of the composed schema. Operations that fail before execution (parsing, validation, authorization) are
   * not reported, as Hive's own clients do.
   */
  val hooks: PhaseHooks[HiveUsage] =
    PhaseHooks.operation(
      PhaseHandler[HiveUsage, Event.Operation, (Long, Long), OperationEvent](event =>
        Clock.currentTime(TimeUnit.MILLISECONDS).zip(Clock.nanoTime).map(event -> _)
      ) { (event, started, outcome) =>
        val (timestamp, start) = started
        ZIO.foreachDiscard(outcome.prepared) { prepared =>
          for {
            end     <- Clock.nanoTime
            headers <- IncomingRequestHeaders.get
            _       <- ZIO.serviceWithZIO[HiveUsage](
                         _.collect(
                           Operation(
                             prepared.document,
                             prepared.executionRequest,
                             event.request.operationName,
                             timestamp,
                             Duration.fromNanos(end - start),
                             outcome.errors.size,
                             ClientInfo.fromHeaders(headers)
                           )
                         )
                       )
          } yield ()
        }
      }
    )
}
