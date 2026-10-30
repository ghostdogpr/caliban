package caliban.hive

import caliban.execution.ExecutionRequest
import caliban.hive.internal.{ Operations, Record, Report }
import caliban.parsing.adt.{ Document, OperationType }
import caliban.wrappers.Wrapper.{ EffectfulWrapper, OverallWrapper, ValidationWrapper }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, IncomingRequestHeaders }
import zio._
import zio.http._

import java.util.concurrent.TimeUnit

/**
 * Reports operation usage to [[https://the-guild.dev/graphql/hive GraphQL Hive]], so its schema registry knows which
 * types and fields clients use.
 *
 * Operations are collected in memory and sent in the background every [[HiveConfig.flushInterval]], and once more when
 * the layer is released. Reporting never fails or delays a request: a full buffer drops operations, and a report Hive
 * does not accept is logged.
 *
 * {{{
 * (api @@ HiveUsage.wrapper).interpreter.provide(HiveUsage.layer(config), Client.default)
 * }}}
 */
trait HiveUsage {

  /** Records one execution, unless it is excluded or sampled out. */
  def collect(operation: HiveUsage.Operation): UIO[Unit]
}

object HiveUsage {

  /**
   * An executed operation as Caliban validated it.
   *
   * @param timestamp when the execution started, in milliseconds since the epoch
   * @param duration until the response was produced; for `@defer` and `@stream` that is the initial payload
   * @param errors the number of GraphQL errors in the response
   */
  final case class Operation(
    document: Document,
    request: ExecutionRequest,
    operationName: Option[String],
    timestamp: Long,
    duration: Duration,
    errors: Int,
    client: Option[ClientInfo]
  ) {
    def subscription: Boolean = request.operationType == OperationType.Subscription
  }

  /** The client that sent an operation, as Hive groups usage by it. */
  final case class ClientInfo(name: String, version: String)

  object ClientInfo {
    private val nameHeaders    = List("x-graphql-client-name", "graphql-client-name")
    private val versionHeaders = List("x-graphql-client-version", "graphql-client-version")

    /** The client named by the headers Hive's own clients read; both name and version must be present. */
    def fromHeaders(headers: List[(String, String)]): Option[ClientInfo] = {
      def first(names: List[String]) =
        names.view.flatMap(name => headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }).headOption
      for {
        name    <- first(nameHeaders)
        version <- first(versionHeaders)
      } yield ClientInfo(name, version)
    }
  }

  /**
   * A [[HiveUsage]] that sends reports with the given client, until the layer is released.
   */
  def layer(config: HiveConfig): ZLayer[Client, Nothing, HiveUsage] =
    ZLayer.scoped(
      for {
        _        <- ZIO
                      .die(new IllegalArgumentException("HiveConfig.maxBatchSize and bufferSize must be at least 1"))
                      .when(config.maxBatchSize < 1 || config.bufferSize < 1)
        client   <- ZIO.service[Client]
        queue    <- Queue.dropping[Operation](config.bufferSize)
        dropped  <- Ref.make(0)
        rejected <- Ref.make(false)
        url       = config.target.split('/').filter(_.nonEmpty).foldLeft(config.endpoint)(_ / _)
        live      = new Live(config, queue, dropped, new Sender(client, url, config, rejected))
        closing  <- Promise.make[Nothing, Unit]
        // Reading `closing` before flushing makes the flush after the signal the last one, so it sends what is left.
        flusher  <- (closing.await.timeout(config.flushInterval) *> closing.isDone.flatMap(live.flush.as(_)))
                      .repeatUntil(identity)
                      .forkScoped
        // Signal instead of interrupting, so a report in flight is not lost. Added after the fork, it runs first.
        _        <- ZIO.addFinalizer(
                      closing.succeed(()) *>
                        flusher.await.interruptible.timeout(config.timeout).flatMap {
                          case Some(Exit.Success(_)) | None => ZIO.unit
                          case Some(_)                      => live.flush.interruptible.timeout(config.timeout)
                        }
                    )
      } yield live
    )

  /**
   * Reports every operation an interpreter executes to the [[HiveUsage]] in the environment.
   */
  val wrapper: EffectfulWrapper[HiveUsage] =
    EffectfulWrapper(ZIO.serviceWith[HiveUsage](usage => validation |+| overall(usage)))

  /** The validated document and request of the current execution, set by [[validation]] for [[overall]]. */
  private val prepared: FiberRef[Option[(Document, ExecutionRequest)]] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(Option.empty[(Document, ExecutionRequest)]))

  private val validation: ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      def wrap[R1 <: Any](
        f: Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest] =
        document => f(document).tap(request => prepared.set(Some(document -> request)))
    }

  private def overall(usage: HiveUsage): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1 <: Any](
        f: GraphQLRequest => URIO[R1, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => URIO[R1, GraphQLResponse[CalibanError]] =
        request =>
          prepared.locally(None)(
            for {
              timestamp <- Clock.currentTime(TimeUnit.MILLISECONDS)
              start     <- Clock.nanoTime
              response  <- f(request)
              end       <- Clock.nanoTime
              validated <- prepared.get
              headers   <- IncomingRequestHeaders.get
              _         <- ZIO.foreachDiscard(validated) { case (document, executionRequest) =>
                             usage.collect(
                               Operation(
                                 document,
                                 executionRequest,
                                 request.operationName,
                                 timestamp,
                                 Duration.fromNanos(end - start),
                                 response.errors.size,
                                 ClientInfo.fromHeaders(headers)
                               )
                             )
                           }
            } yield response
          )
    }

  private final class Live(config: HiveConfig, queue: Queue[Operation], dropped: Ref[Int], sender: Sender)
      extends HiveUsage {

    def collect(operation: Operation): UIO[Unit] = {
      val excluded =
        config.exclude.nonEmpty &&
          Operations.name(operation.document, operation.operationName).exists(config.exclude.contains)
      ZIO.unlessDiscard(excluded) {
        ZIO.whenZIODiscard(sampled)(queue.offer(operation).flatMap(offered => dropped.update(_ + 1).unless(offered)))
      }
    }

    private val sampled: UIO[Boolean] =
      if (config.sampleRate >= 1.0) ZIO.succeed(true) else Random.nextDouble.map(_ < config.sampleRate)

    /** Sends everything collected so far, in batches of at most [[HiveConfig.maxBatchSize]]. */
    val flush: UIO[Unit] =
      dropped.getAndSet(0).flatMap { count =>
        ZIO.logWarning(s"Dropped $count operations because the Hive usage buffer was full").when(count > 0)
      } *>
        queue
          .takeUpTo(config.maxBatchSize)
          .flatMap(batch => sender.send(batch.flatMap(op => Chunk.fromIterable(Record.of(op)))).as(batch.size))
          .repeatWhile(_ == config.maxBatchSize)
          .unit
  }

  private final class Sender(client: Client, url: URL, config: HiveConfig, rejected: Ref[Boolean]) {

    /**
     * Retries what may succeed later (the connection, a timeout, a 5xx or a 429). Logs what Hive rejects, as a warning
     * the first time only, since a wrong token is rejected on every flush.
     */
    def send(records: Chunk[Record]): UIO[Unit] =
      ZIO.unlessDiscard(records.isEmpty) {
        val request = Request
          .post(url, Body.fromString(Report.encode(records)))
          .addHeaders(
            Headers(
              Header.Custom("Authorization", s"Bearer ${config.token.stringValue}"),
              Header.ContentType(MediaType.application.json),
              Header.Custom("X-Usage-API-Version", "2")
            )
          )
        client
          .batched(request)
          .timeoutFail(new RuntimeException(s"Hive did not answer within ${config.timeout.render}"))(config.timeout)
          .flatMap { response =>
            val status = response.status
            if (status.isSuccess) ZIO.unit
            else if (status.isServerError || status == Status.TooManyRequests)
              ZIO.fail(new RuntimeException(s"Hive answered ${status.code}"))
            else
              response.body.asString.orElseSucceed("").flatMap { body =>
                val message = s"Hive rejected a usage report of ${records.size} operations: ${status.code} $body"
                rejected
                  .getAndSet(true)
                  .flatMap(before => if (before) ZIO.logDebug(message) else ZIO.logWarning(message))
              }
          }
          .retry(Schedule.exponential(1.second) && Schedule.recurs(2))
          .catchAllCause(cause => ZIO.logWarningCause(s"Could not report ${records.size} operations to Hive", cause))
      }
  }
}
