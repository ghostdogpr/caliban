package caliban.client

import scala.annotation.tailrec

trait Renderer[-A, S, -Options] { self =>
  type State = S

  final def render(a: A, state0: State, options: Options): (String, State) = {
    val sb     = new StringBuilder()
    val state1 = unsafeRender(a, state0, sb, options)
    (sb.toString, state1)
  }

  def unsafeRender(
    a: A,
    state: State,
    writer: StringBuilder,
    options: Options
  ): State

  def contramap[A0](f: A0 => A): Renderer[A0, S, Options] =
    new Renderer[A0, S, Options] {
      override def unsafeRender(a: A0, state: State, writer: StringBuilder, options: Options): State =
        self.unsafeRender(f(a), state, writer, options)
    }

}

object Renderer {

  def empty[S]: Renderer[Any, S, Any] =
    Empty.asInstanceOf[Renderer[Any, S, Any]]

  private case object Empty extends Renderer[Any, Any, Any] {
    override def unsafeRender(a: Any, state: Any, writer: StringBuilder, options: Any): Any =
      state
  }

  def list[A, S, O](renderer: Renderer[A, S, O], separator: Char): Renderer[List[A], S, O] =
    new Renderer[List[A], S, O] {
      override def unsafeRender(a: List[A], state0: S, writer: StringBuilder, options: O): S = {
        @tailrec
        def loop(remaining: List[A], state: S, first: Boolean): S = remaining match {
          case Nil          => state
          case head :: tail =>
            if (!first) writer.append(separator)
            val s0 = renderer.unsafeRender(head, state, writer, options)
            loop(tail, s0, first = false)
        }

        loop(a, state0, first = true)
      }
    }

  def set[A, S, O](renderer: Renderer[A, S, O], separator: Char): Renderer[Set[A], S, O] =
    list(renderer, separator).contramap(_.toList)
}
