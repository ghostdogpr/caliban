package caliban.interop.tapir

import caliban.{ GraphQLWSOutput, InputValue, ResponseValue }
import zio.ZIO
import zio.stream.ZStream

trait StreamTransformer[-R, +E] {
  def transform[R1 <: R, E1 >: E](stream: ZStream[R1, E1, GraphQLWSOutput]): ZStream[R1, E1, GraphQLWSOutput]
}

trait WebSocketHooks[-R, +E] { self =>
  def beforeInit: Option[InputValue => ZIO[R, E, Any]] = None
  def afterInit: Option[ZIO[R, E, Any]]                = None
  def onMessage: Option[StreamTransformer[R, E]]       = None
  def onPong: Option[InputValue => ZIO[R, E, Any]]     = None
  def onAck: Option[ZIO[R, E, ResponseValue]]          = None

  def ++[R2 <: R, E2 >: E](other: WebSocketHooks[R2, E2]): WebSocketHooks[R2, E2] =
    new WebSocketHooks[R2, E2] {
      override def beforeInit: Option[InputValue => ZIO[R2, E2, Any]] = (self.beforeInit, other.beforeInit) match {
        case (None, Some(f))      => Some(f)
        case (Some(f), None)      => Some(f)
        case (Some(f1), Some(f2)) => Some((x: InputValue) => f1(x) *> f2(x))
        case _                    => None
      }

      override def afterInit: Option[ZIO[R2, E2, Any]] = (self.afterInit, other.afterInit) match {
        case (None, Some(f))      => Some(f)
        case (Some(f), None)      => Some(f)
        case (Some(f1), Some(f2)) => Some(f1 &> f2)
        case _                    => None
      }

      override def onMessage: Option[StreamTransformer[R2, E2]] =
        (self.onMessage, other.onMessage) match {
          case (None, Some(f))      => Some(f)
          case (Some(f), None)      => Some(f)
          case (Some(f1), Some(f2)) =>
            Some(new StreamTransformer[R2, E2] {
              def transform[R1 <: R2, E1 >: E2](s: ZStream[R1, E1, GraphQLWSOutput]): ZStream[R1, E1, GraphQLWSOutput] =
                f2.transform(f1.transform(s))
            })
          case _                    => None
        }

      override def onPong: Option[InputValue => ZIO[R2, E2, Any]] = (self.onPong, other.onPong) match {
        case (None, Some(f))      => Some(f)
        case (Some(f), None)      => Some(f)
        case (Some(f1), Some(f2)) => Some((x: InputValue) => f1(x) &> f2(x))
        case _                    => None
      }
    }
}

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
}
