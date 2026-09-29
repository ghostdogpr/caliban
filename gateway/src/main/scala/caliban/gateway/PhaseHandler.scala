package caliban.gateway

import zio.{ Exit, Scope, Trace, ZIO }

/**
 * A PhaseHandler is an injectable handler for a specific phase of the gateway execution.
 * For the exact injection points see [[PhaseHooks]] which defines the set of available hooks.
 *
 * On the incoming side a handler receives the phase's event and may modify it; the modified event is what reaches
 * the injection point. On the outgoing side it receives the event its own incoming side produced, its own context,
 * and the result of the wrapped execution.
 *
 * PhaseHandlers can be composed sequentially using the `++` operator.
 */
sealed abstract class PhaseHandler[-R, Event, +Err, -Res] { self =>
  import PhaseHandler.Combined

  /**
   * Composes two phase handlers sequentially. The first handler's incoming side runs first, and the event it produces
   * is the input to the second handler. Outgoing sides run in reverse order, so the second handler's outgoing side
   * runs before the first's.
   */
  def ++[R1 <: R, Err1 >: Err, Res1 <: Res](
    that: PhaseHandler[R1, Event, Err1, Res1]
  ): PhaseHandler[R1, Event, Err1, Res1] =
    if (!self.enabled) that else if (!that.enabled) self else Combined(self, that)

  /**
   * Whether this phase handler is enabled. A disabled handler is skipped during gateway execution.
   */
  final def enabled: Boolean =
    this match {
      case PhaseHandler.Empty() => false
      case _                    => true
    }

  private[gateway] final def hasIncoming: Boolean =
    this match {
      case PhaseHandler.Incoming(_)            => true
      case PhaseHandler.IncomingOutgoing(_, _) => true
      case PhaseHandler.Scoped(handler)        => handler.hasIncoming
      case PhaseHandler.Combined(first, next)  => first.hasIncoming || next.hasIncoming
      case _                                   => false
    }

  /**
   * Runs the phase handler, if enabled. It receives the initial event, an effect to wrap and a conversion function
   * to convert the result of the wrapped effect into this handler's result type.
   */
  final def run[R1 <: R, E >: Err, A](event: => Event)(effect: ZIO[R1, E, A])(
    result: Exit[E, A] => Res
  )(implicit trace: Trace): ZIO[R1, E, A] =
    if (!enabled) effect
    else runWith[R1, E, A](event)(_ => effect)(result)

  /**
   * Runs the phase handler, if enabled. It receives the initial event, a function to wrap, which itself receives the
   * event the incoming side produced, and a conversion function to convert the result of the wrapped effect into this
   * handler's result type.
   */
  def runWith[R1 <: R, E >: Err, A](event: Event)(fn: Event => ZIO[R1, E, A])(result: Exit[E, A] => Res)(implicit
    trace: Trace
  ): ZIO[R1, E, A]
}

object PhaseHandler {

  /**
   * Constructs a PhaseHandler with an incoming and outgoing phase as well as a context type
   * that can be used to pass information between hooks.
   */
  def apply[R, Ev, Err, Ctx0, Out](incoming: Ev => ZIO[R, Err, (Ev, Ctx0)])(
    outgoing: (Ev, Ctx0, Out) => ZIO[R, Nothing, Unit]
  ): PhaseHandler[R, Ev, Err, Out] = IncomingOutgoing(incoming, outgoing)

  /**
   * Constructs a PhaseHandler that does not perform any incoming or outgoing hooks.
   */
  def empty[Ev]: PhaseHandler[Any, Ev, Nothing, Any] = Empty()

  /**
   * Constructs a PhaseHandler that only performs an incoming phase, for modifying the incoming event, running
   * pre-processing side-effects, or short-circuiting the wrapped phase by failing.
   */
  def incoming[R, Ev, Err](incoming: Ev => ZIO[R, Err, Ev]): PhaseHandler[R, Ev, Err, Any] =
    Incoming(incoming)

  /**
   * Similar to [[incoming]] but does not modify the incoming event.
   */
  def incomingDiscard[R, Ev, Err](incoming: Ev => ZIO[R, Err, Unit]): PhaseHandler[R, Ev, Err, Any] =
    Incoming((event: Ev) => incoming(event).as(event))

  /**
   * Constructs a PhaseHandler that only performs an outgoing phase, for post-processing side-effects. It cannot
   * modify the wrapped event and it cannot fail.
   */
  def outgoing[R, Ev, Out](outgoing: (Ev, Out) => ZIO[R, Nothing, Unit]): PhaseHandler[R, Ev, Nothing, Out] =
    Outgoing(outgoing)

  /**
   * Constructs a PhaseHandler that wraps another PhaseHandler which has a Scope requirement. This constructor
   * consumes the Scope so that it is bound to the lifespan of the hook.
   */
  def scoped[R, Ev, Err, Out](handler: PhaseHandler[Scope with R, Ev, Err, Out]): PhaseHandler[R, Ev, Err, Out] =
    if (handler.enabled) Scoped[R, Ev, Err, Out](handler) else empty[Ev]

  private final case class Empty[Ev]() extends PhaseHandler[Any, Ev, Nothing, Any] {
    def runWith[R1 <: Any, E >: Nothing, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Any
    )(implicit trace: Trace): ZIO[R1, E, A] = fn(event)
  }

  private case class Incoming[-R, Ev, +Err](incoming: Ev => ZIO[R, Err, Ev]) extends PhaseHandler[R, Ev, Err, Any] {
    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Any
    )(implicit trace: Trace): ZIO[R1, E, A] = incoming(event).flatMap(fn)
  }

  private case class Outgoing[-R, Ev, Out](outgoing: (Ev, Out) => ZIO[R, Nothing, Unit])
      extends PhaseHandler[R, Ev, Nothing, Out] {
    def runWith[R1 <: R, E >: Nothing, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] =
      fn(event).onExit(exit => outgoing(event, result(exit)))
  }

  private case class IncomingOutgoing[-R, Ev, +Err, Out, Ctx0](
    incoming: Ev => ZIO[R, Err, (Ev, Ctx0)],
    outgoing: (Ev, Ctx0, Out) => ZIO[R, Nothing, Unit]
  ) extends PhaseHandler[R, Ev, Err, Out] {
    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] = ZIO.uninterruptibleMask { restore =>
      restore(incoming(event)).flatMap { case (updated, ctx) =>
        restore(fn(updated)).onExit(exit => outgoing(updated, ctx, result(exit)))
      }
    }
  }

  private case class Scoped[-R, Ev, +Err, Out](handler: PhaseHandler[Scope with R, Ev, Err, Out])
      extends PhaseHandler[R, Ev, Err, Out] {

    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] =
      // The scope has to outlive the handler's own outgoing callback, which runs inside `runWith`, so it cannot be
      // shared with the rest of the chain and closed at the end of it.
      ZIO.scoped[R1](handler.runWith[Scope with R1, E, A](event)(fn)(result))
  }

  private case class Combined[-R, Ev, +Err, Out](
    first: PhaseHandler[R, Ev, Err, Out],
    next: PhaseHandler[R, Ev, Err, Out]
  ) extends PhaseHandler[R, Ev, Err, Out] {

    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] =
      first.runWith[R1, E, A](event)(next.runWith[R1, E, A](_)(fn)(result))(result)
  }
}
