package caliban.gateway

import zio.{ Exit, Scope, Trace, ZIO }

import scala.collection.mutable.ListBuffer

/**
 * A PhaseHandler is an injectable handler for a specific phase of the gateway execution.
 * For the exact injection points see [[PhaseHooks]] which defines the set of available hooks.
 *
 * On the incoming side a handler receives the phase's event and may modify it; the modified event is what reaches
 * the injection point. On the outgoing side it receives the final event, its own context, and the result of the
 * wrapped execution.
 *
 * PhaseHandlers can be composed sequentially using the `++` operator.
 */
sealed abstract class PhaseHandler[-R, Event, +Err, -Res] { self =>
  import PhaseHandler.Combined

  /**
   * Composes two phase handlers sequentially. The first handler runs first, and the event its incoming side produces
   * is the input to the second handler. Both handlers receive the final event in their outgoing phase.
   */
  def ++[R1 <: R, Err1 >: Err, Res1 <: Res](
    that: PhaseHandler[R1, Event, Err1, Res1]
  ): PhaseHandler[R1, Event, Err1, Res1] =
    if (self == PhaseHandler.empty) that
    else if (that == PhaseHandler.empty) self
    else
      (self, that) match {
        case (Combined(left), Combined(right)) => Combined(left ++ right)
        case (Combined(handlers), that)        => Combined(handlers :+ that)
        case (self, Combined(handlers))        => Combined(self +: handlers)
        case (left, right)                     => Combined(List(left, right))
      }

  /**
   * Whether this phase handler is enabled. A disabled handler is skipped during gateway execution.
   */
  def enabled: Boolean

  /**
   * Runs the phase handler, if enabled. It receives the initial event, an effect to wrap and a conversion function
   * to convert the result of the wrapped effect into this handler's result type.
   */
  final def run[R1 <: R, E >: Err, A](event: Event)(effect: ZIO[R1, E, A])(
    result: Exit[E, A] => Res
  )(implicit trace: Trace): ZIO[R1, E, A] =
    if (!enabled) effect
    else runWith[R1, E, A](event)(_ => effect)(result)

  /**
   * Runs the phase handler, if enabled. It receives the initial event, a function to wrap, which itself receives the final event,
   * and a conversion function to convert the result of the wrapped effect into this handler's result type.
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
  def empty[Ev]: PhaseHandler[Any, Ev, Nothing, Any] = Empty.asInstanceOf[PhaseHandler[Any, Ev, Nothing, Any]]

  /**
   * Constructs a PhaseHandler that only performs an incoming phase, for modifying the incoming event, running
   * pre-processing side-effects, or short-circuiting the wrapped phase by failing.
   */
  def incoming[R, Ev, Err](
    incoming: Ev => ZIO[R, Err, Ev]
  ): PhaseHandler[R, Ev, Err, Any] =
    Incoming(incoming)

  /**
   * Similar to [[incoming]] but does not modify the incoming event.
   */
  def incomingDiscard[R, Ev, Err](
    incoming: Ev => ZIO[R, Err, Unit]
  ): PhaseHandler[R, Ev, Err, Any] =
    Incoming((ev: Ev) => incoming(ev).as(ev))

  /**
   * Constructs a PhaseHandler that only performs an outgoing phase, for post-processing side-effects. It cannot
   * modify the wrapped event and it cannot fail.
   */
  def outgoing[R, Ev, Out](
    outgoing: (Ev, Out) => ZIO[R, Nothing, Unit]
  ): PhaseHandler[R, Ev, Nothing, Out] =
    Outgoing(outgoing)

  /**
   * Constructs a PhaseHandler that wraps another PhaseHandler which has a Scope requirement. This constructor
   * consumes the Scope so that it is bound to the lifespan of the hook.
   */
  def scoped[R, Ev, Err, Out](handler: PhaseHandler[Scope with R, Ev, Err, Out]): PhaseHandler[R, Ev, Err, Out] =
    Scoped[R, Ev, Err, Out](handler)

  private case object Empty extends PhaseHandler[Any, Any, Nothing, Any] {
    override val enabled: Boolean = false

    def runWith[R1 <: Any, E >: Nothing, A](event: Any)(fn: Any => ZIO[R1, E, A])(
      result: Exit[E, A] => Any
    )(implicit trace: Trace): ZIO[R1, E, A] = fn(event)
  }

  private case class Incoming[-R, Ev, +Err](incoming: Ev => ZIO[R, Err, Ev]) extends PhaseHandler[R, Ev, Err, Any] {
    override val enabled: Boolean = true

    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Any
    )(implicit trace: Trace): ZIO[R1, E, A] = incoming(event).flatMap(fn)
  }

  private case class Outgoing[-R, Ev, Out](outgoing: (Ev, Out) => ZIO[R, Nothing, Unit])
      extends PhaseHandler[R, Ev, Nothing, Out] {
    override val enabled: Boolean = true

    def runWith[R1 <: R, E >: Nothing, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] =
      fn(event).onExit(exit => outgoing(event, result(exit)))
  }

  private case class IncomingOutgoing[-R, Ev, +Err, Out, Ctx0](
    incoming: Ev => ZIO[R, Err, (Ev, Ctx0)],
    outgoing: (Ev, Ctx0, Out) => ZIO[R, Nothing, Unit]
  ) extends PhaseHandler[R, Ev, Err, Out] {
    override val enabled: Boolean = true

    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] = ZIO.uninterruptibleMask { restore =>
      incoming(event).flatMap { case (ev, ctx) =>
        restore(fn(ev)).onExit(exit => outgoing(ev, ctx, result(exit)))
      }
    }
  }

  private case class Scoped[-R, Ev, +Err, Out](handler: PhaseHandler[Scope with R, Ev, Err, Out])
      extends PhaseHandler[R, Ev, Err, Out] {

    def enabled: Boolean = handler.enabled

    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] =
      ZIO.scoped[R1](handler.runWith[Scope with R1, E, A](event)(fn)(result))
  }

  private case class Combined[-R, Ev, +Err, Out](
    handlers: List[PhaseHandler[R, Ev, Err, Out]]
  ) extends PhaseHandler[R, Ev, Err, Out] {

    def enabled: Boolean = handlers.exists(_.enabled)

    def runWith[R1 <: R, E >: Err, A](event: Ev)(fn: Ev => ZIO[R1, E, A])(
      result: Exit[E, A] => Out
    )(implicit trace: Trace): ZIO[R1, E, A] = {
      def loop[R2 <: R1](
        remaining: List[PhaseHandler[R2, Ev, Err, Out]],
        ev0: Ev
      ): ZIO[R2, E, A] =
        remaining match {
          case Nil                        => fn(ev0)
          case Incoming(incoming) :: next => incoming(ev0).flatMap(loop(next, _))
          case Combined(handlers) :: next => loop(handlers ++ next, ev0)
          // The scope has to outlive the handler's own outgoing callback, which runs inside `runWith`, so it cannot be
          // shared with the rest of the chain and closed at the end of it.
          case Scoped(handler) :: next    => ZIO.scoped[R2](loop[R2 with Scope](handler :: next, ev0))
          case handler :: next            => handler.runWith[R2, E, A](ev0)(loop(next, _))(result)
        }

      loop(handlers, event)
    }
  }
}
