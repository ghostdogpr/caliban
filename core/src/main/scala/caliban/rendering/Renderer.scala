package caliban.rendering

import scala.annotation.switch

/**
 * The inverse of a `Parser` over some type A.
 * A renderer can be used to render a value of type A to a string in either a regular or compact format.
 *
 * For specializations actually relevant to graphql see [[caliban.rendering.ValueRenderer]] and [[caliban.rendering.DocumentRenderer]]
 */
trait Renderer[-A] { self =>
  def render(a: A): String = {
    val sb = new StringBuilder
    unsafeRender(a, Some(0), sb)
    sb.toString()
  }

  def renderCompact(a: A): String = {
    val sb = new StringBuilder
    unsafeRender(a, None, sb)
    sb.toString()
  }

  /**
   * Combines this renderer with another renderer sequentially. Semantically equivalent to `this andThen that`
   */
  def ++[A1 <: A](that: Renderer[A1]): Renderer[A1] = self match {
    case Renderer.Combined(renderers) => Renderer.Combined(renderers :+ that)
    case _                            => Renderer.Combined(List(self, that))
  }

  /**
   * Contramaps the input of this renderer with the given function producing a renderer that now operates on type B
   */
  def contramap[B](f: B => A): Renderer[B] = new Renderer[B] {
    override def unsafeRender(value: B, indent: Option[Int], write: StringBuilder): Unit =
      self.unsafeRender(f(value), indent, write)
  }

  /**
   * Returns an optional renderer that will only render the value if it is defined
   */
  def optional: Renderer[Option[A]] = new Renderer[Option[A]] {
    override def unsafeRender(value: Option[A], indent: Option[Int], write: StringBuilder): Unit =
      value.foreach(self.unsafeRender(_, indent, write))
  }

  /**
   * Returns a renderer that renders a list of A where the underlying renderer is responsible for rendering the
   * separator between each element.
   */
  def list: Renderer[List[A]] =
    list(Renderer.empty)

  /**
   * Returns a renderer that renders a list of A but where the separator is rendered by provided argument renderer.
   * The second parameter determines whether to print the separator before the first element or not.
   */
  def list[A1 <: A](separator: Renderer[A1], omitFirst: Boolean = true): Renderer[List[A1]] = new Renderer[List[A1]] {
    override protected[caliban] def unsafeRender(value: List[A1], indent: Option[Int], write: StringBuilder): Unit = {
      var first = omitFirst
      value.foreach { v =>
        if (first) first = false
        else separator.unsafeRender(v, indent, write)
        self.unsafeRender(v, indent, write)
      }
    }
  }

  /**
   * Returns a renderer that renders a set of A but where the separator is rendered by provided argument renderer.
   */
  def set[A1 <: A](separator: Renderer[A1]): Renderer[Set[A1]] = new Renderer[Set[A1]] {
    override protected[caliban] def unsafeRender(value: Set[A1], indent: Option[Int], write: StringBuilder): Unit = {
      var first = true
      value.foreach { v =>
        if (first) first = false
        else separator.unsafeRender(v, indent, write)
        self.unsafeRender(v, indent, write)
      }
    }
  }

  /**
   * Returns a renderer that will only render when the provided predicate is true.
   */
  def when[A1 <: A](pred: A1 => Boolean): Renderer[A1] = new Renderer[A1] {
    override protected[caliban] def unsafeRender(value: A1, indent: Option[Int], write: StringBuilder): Unit =
      if (pred(value)) self.unsafeRender(value, indent, write)
  }

  /**
   * Protected method for implementers to override. This method provides the actual unsafe rendering logic.
   * @param value the value to render
   * @param indent the current indentation level. This will be None if the renderer is rendering in compact mode or Some(n)
   *               if the renderer is rendering in regular mode where n is the current indentation level.
   * @param write the string builder to write to
   */
  protected[caliban] def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit
}

object Renderer {

  def combine[A](renderers: Renderer[A]*): Renderer[A] =
    Combined(renderers.toList)

  /**
   * A Renderer which always renders a single character.
   */
  def char(char: Char): Renderer[Any] = new Renderer[Any] {
    override def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      write.append(char)
  }

  def comma: Renderer[Any] = char(',')

  /**
   * A Renderer which always renders a string.
   */
  def string(str: String): Renderer[Any] = new Renderer[Any] {
    override def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      write.append(str)
  }

  /**
   * A Renderer which simply renders the input string
   */
  lazy val string: Renderer[String] = new Renderer[String] {
    override def unsafeRender(value: String, indent: Option[Int], write: StringBuilder): Unit =
      write.append(value)
  }

  lazy val escapedString: Renderer[String] = new Renderer[String] {
    override def unsafeRender(value: String, indent: Option[Int], write: StringBuilder): Unit =
      unsafeFastEscape(value, write)

    private def unsafeFastEscape(value: String, writer: StringBuilder): Unit = {
      var i = 0
      while (i < value.length) {
        (value.charAt(i): @switch) match {
          case '\\' => writer.append("\\\\")
          case '\b' => writer.append("\\b")
          case '\f' => writer.append("\\f")
          case '\n' => writer.append("\\n")
          case '\r' => writer.append("\\r")
          case '\t' => writer.append("\\t")
          case '"'  => writer.append("\\\"")
          case c    => writer.append(c)
        }
        i += 1
      }
    }
  }

  /**
   * A Renderer which doesn't render anything.
   */
  lazy val empty: Renderer[Any] = new Renderer[Any] {
    override def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit = ()
  }

  /**
   * A Renderer which renders a space character when in non-compact mode.
   */
  lazy val spaceOrEmpty: Renderer[Any] = new Renderer[Any] {

    override protected[caliban] def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      if (indent.isDefined) write.append(' ')
  }

  /**
   * A Renderer which renders a newline character when in non-compact mode otherwise it renders a comma
   */
  lazy val newlineOrComma: Renderer[Any] = new Renderer[Any] {
    override protected[caliban] def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      if (indent.isDefined) write.append('\n') else write.append(',')
  }

  /**
   * A Renderer which renders a newline character when in non-compact mode otherwise it renders a space
   */
  lazy val newlineOrSpace: Renderer[Any] = new Renderer[Any] {
    override protected[caliban] def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      if (indent.isDefined) write.append('\n') else write.append(' ')
  }

  lazy val newlineOrEmpty: Renderer[Any] = new Renderer[Any] {
    override protected[caliban] def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      if (indent.isDefined) write.append('\n')
  }

  lazy val newline: Renderer[Any] = char('\n')

  def map[K, V](
    keyRender: Renderer[K],
    valueRender: Renderer[V],
    separator: Renderer[Any],
    delimiter: Renderer[Any]
  ): Renderer[Map[K, V]] = new Renderer[Map[K, V]] {
    override def unsafeRender(value: Map[K, V], indent: Option[Int], write: StringBuilder): Unit = {
      var first = true
      value.foreach { case (k, v) =>
        if (first) first = false
        else separator.unsafeRender((), indent, write)
        keyRender.unsafeRender(k, indent, write)
        delimiter.unsafeRender((), indent, write)
        valueRender.unsafeRender(v, indent, write)
      }
    }
  }

  private final case class Combined[-A](renderers: List[Renderer[A]]) extends Renderer[A] {
    override def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit =
      renderers.foreach(_.unsafeRender(value, indent, write))
  }
}
