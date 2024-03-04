package caliban.interop.tapir

import caliban.{ InputValue, ResponseValue }
import zio.ZIO

@deprecated("Use caliban.ws.StreamTransformer instead", "2.6.0")
trait StreamTransformer[-R, +E] extends caliban.ws.StreamTransformer[R, E]

@deprecated("Use caliban.ws.WebSocketHooks instead", "2.6.0")
trait WebSocketHooks[-R, +E] extends caliban.ws.WebSocketHooks[R, E]

@deprecated("Use caliban.ws.WebSocketHooks instead", "2.6.0")
object WebSocketHooks {
  def empty[R, E]: WebSocketHooks[R, E] = new WebSocketHooks[R, E] {}

  /**
   * Specifies a callback that will be run before an incoming subscription
   * request is accepted. Useful for e.g authorizing the incoming subscription
   * before accepting it.
   */
  def init[R, E](f: InputValue => ZIO[R, E, Any]): WebSocketHooks[R, E] =
    new WebSocketHooks[R, E] {
      override def beforeInit: Option[InputValue => ZIO[R, E, Any]] = Some(f)
    }

  /**
   * Specifies a callback that will be run after an incoming subscription
   * request has been accepted. Useful for e.g terminating a subscription
   * after some time, such as authorization expiring.
   */
  def afterInit[R, E](f: ZIO[R, E, Any]): WebSocketHooks[R, E] =
    new WebSocketHooks[R, E] {
      override def afterInit: Option[ZIO[R, E, Any]] = Some(f)
    }

  /**
   * Specifies a callback that will be run on the resulting `ZStream`
   * for every active subscription. Useful to e.g modify the environment
   * to inject session information into the `ZStream` handling the
   * subscription.
   */
  def message[R, E](f: StreamTransformer[R, E]): WebSocketHooks[R, E] =
    new WebSocketHooks[R, E] {
      override def onMessage: Option[StreamTransformer[R, E]] = Some(f)
    }

  /**
   * Specifies a callback that will be run when ever a pong message is received.
   */
  def pong[R, E](f: InputValue => ZIO[R, E, Any]): WebSocketHooks[R, E] =
    new WebSocketHooks[R, E] {
      override def onPong: Option[InputValue => ZIO[R, E, Any]] = Some(f)
    }

  def ack[R, E](f: ZIO[R, E, ResponseValue]): WebSocketHooks[R, E] =
    new WebSocketHooks[R, E] {
      override def onAck: Option[ZIO[R, E, ResponseValue]] = Some(f)
    }
}
