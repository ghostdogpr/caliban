package caliban.hive

import zio._
import zio.http._

/** A fake Hive usage API that keeps every report it receives, for tests here and in `caliban-gateway-hive`. */
object UsageEndpoint {

  final case class Received(requests: Ref[Vector[Request]], bodies: Ref[Vector[String]])

  private val ids = new java.util.concurrent.atomic.AtomicInteger()

  /**
   * Serves `POST /usage-<n>/...` and returns its base URL, to use as [[HiveConfig.endpoint]]. Each endpoint has its
   * own path, so tests can share a server. It answers the n-th report with the n-th status, then with the last one.
   */
  def start(statuses: Status*): ZIO[Server, Nothing, (URL, Received)] = startWith(ZIO.unit, statuses: _*)

  /** Like [[start]], but runs `beforeAnswer` after recording each report and before answering it. */
  def startWith(beforeAnswer: UIO[Unit], statuses: Status*): ZIO[Server, Nothing, (URL, Received)] =
    for {
      requests <- Ref.make(Vector.empty[Request])
      bodies   <- Ref.make(Vector.empty[String])
      base     <- ZIO.succeed(s"usage-${ids.incrementAndGet()}")
      routes    =
        Routes(Method.POST / base / trailing -> handler { (_: Path, request: Request) =>
          request.body.asString.orDie.flatMap(body =>
            requests.modify(rs => (rs.size, rs :+ request)).flatMap { index =>
              val status = statuses.lift(index).orElse(statuses.lastOption).getOrElse(Status.Ok)
              bodies.update(_ :+ body) *> beforeAnswer.as(Response.status(status))
            }
          )
        })
      port     <- Server.install(routes)
      url      <- ZIO.fromEither(URL.decode(s"http://localhost:$port/$base")).orDie
    } yield (url, Received(requests, bodies))
}
