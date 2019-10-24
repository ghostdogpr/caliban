package epstein

/**
 * A `Request[A]` is a request from a data source for a value of type `A`.
 *
 * {{{
 * sealed trait UserRequest[+A] extends Request[A]
 *
 * case object GetAllIds                 extends UserRequest[List[Int]]
 * final case class GetNameById(id: Int) extends UserRequest[String]
 *
 * }}}
 */
trait Request[+A]
