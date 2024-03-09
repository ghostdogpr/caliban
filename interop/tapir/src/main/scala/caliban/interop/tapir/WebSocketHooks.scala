package caliban.interop.tapir

import caliban.{ GraphQLWSOutput, InputValue, ResponseValue }
import zio.ZIO
import zio.stream.{ ZPipeline, ZStream }

@deprecated(
  "WebSocketHooks.onMessage now uses a ZPipeline instead. To convert your existing logic into a ZPipeline, use `ZPipeline.fromFunction`",
  "2.6.0"
)
trait StreamTransformer[-R, +E] {
  def transform[R1 <: R, E1 >: E](stream: ZStream[R1, E1, GraphQLWSOutput]): ZStream[R1, E1, GraphQLWSOutput]
}

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
      override def onMessage: Option[ZPipeline[R, E, GraphQLWSOutput, GraphQLWSOutput]] =
        Some(ZPipeline.fromFunction(f.transform))
    }

  /**
   * Specifies a ZPipeline that will be applied on the resulting `ZStream`
   * for every active subscription. Useful to e.g modify the environment
   * to inject session information into the `ZStream` handling the
   * subscription.
   */
  def message[R, E](f: ZPipeline[R, E, GraphQLWSOutput, GraphQLWSOutput]): WebSocketHooks[R, E] =
    new WebSocketHooks[R, E] {
      override def onMessage: Option[ZPipeline[R, E, GraphQLWSOutput, GraphQLWSOutput]] = Some(f)
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
