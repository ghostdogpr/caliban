package caliban.rendering

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

  def contramap[B](f: B => A): Renderer[B] = new Renderer[B] {
    override def unsafeRender(value: B, indent: Option[Int], write: StringBuilder): Unit =
      self.unsafeRender(f(value), indent, write)
  }

  /**
   * Returns a new Renderer which will only render if it is not running in compact mode
   */
  def ignoreCompact: Renderer[A] = new Renderer[A] {
    override def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit =
      if (indent.isDefined) self.unsafeRender(value, None, write)
  }

  def optional: Renderer[Option[A]] = new Renderer[Option[A]] {
    override def unsafeRender(value: Option[A], indent: Option[Int], write: StringBuilder): Unit =
      value.foreach(self.unsafeRender(_, indent, write))
  }

  def list: Renderer[List[A]] = new Renderer[List[A]] {
    override def unsafeRender(value: List[A], indent: Option[Int], write: StringBuilder): Unit =
      value.foreach(self.unsafeRender(_, indent, write))
  }

  def list(separator: String): Renderer[List[A]] = new Renderer[List[A]] {
    override def unsafeRender(value: List[A], indent: Option[Int], write: StringBuilder): Unit = {
      var first = true
      value.foreach { v =>
        if (first) first = false
        else write.append(separator)
        self.unsafeRender(v, indent, write)
      }
    }
  }

  def when[A1 <: A](f: A1 => Boolean): Renderer[A1] = new Renderer[A1] {
    override def unsafeRender(value: A1, indent: Option[Int], write: StringBuilder): Unit =
      if (f(value)) self.unsafeRender(value, indent, write)
  }

  protected[caliban] def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit
}

object Renderer {

  def combine[A](renderers: Renderer[A]*): Renderer[A] = new Renderer[A] {
    override def unsafeRender(value: A, indent: Option[Int], write: StringBuilder): Unit =
      renderers.foreach(_.unsafeRender(value, indent, write))
  }

  def string(str: String): Renderer[Any] = new Renderer[Any] {
    override def unsafeRender(value: Any, indent: Option[Int], write: StringBuilder): Unit =
      write.append(str)
  }

}
