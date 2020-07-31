package caliban.client.apq

import sttp.client.monad.MonadError
import sttp.client.ws.WebSocketResponse
import sttp.client.{ Request, Response, SttpBackend }
import zio.{ RIO, Ref }

import scala.language.higherKinds

/**
 * Simple backend which "counts" the requests that pass through it and applies them to a tag
 * This is useful for mocking when you want to keep track of response orders
 */
class SttpCounterBackend[R, -S, -WS[_]](other: SttpBackend[RIO[R, *], S, WS], counter: Ref[Int])
    extends SttpBackend[RIO[R, *], S, WS] {

  override def send[T](request: Request[T, S]): RIO[R, Response[T]] =
    for {
      count <- counter.getAndUpdate(_ + 1)
      resp  <- other.send(request.tag("counter", count))
    } yield resp

  override def openWebsocket[T, WS_RESULT](
    request: Request[T, S],
    handler: WS[WS_RESULT]
  ): RIO[R, WebSocketResponse[WS_RESULT]] =
    other.openWebsocket(request, handler)

  override def close(): RIO[R, Unit] = other.close()

  override def responseMonad: MonadError[RIO[R, *]] = other.responseMonad
}

object SttpCounterBackend {

  def apply[R, S, WS[_]](other: SttpBackend[RIO[R, *], S, WS], ref: Ref[Int]): SttpBackend[RIO[R, *], S, WS] =
    new SttpCounterBackend(other, ref)

}
