package caliban.client

trait ArgEncoder[-A] {
  def encode(value: A): String
}

object ArgEncoder {

  implicit val int: ArgEncoder[Int]         = (value: Int) => value.toString
  implicit val string: ArgEncoder[String]   = (value: String) => s""""$value""""
  implicit val boolean: ArgEncoder[Boolean] = (value: Boolean) => value.toString

  implicit def option[A](implicit ev: ArgEncoder[A]): ArgEncoder[Option[A]] =
    (value: Option[A]) => value.fold("")(ev.encode)

}
