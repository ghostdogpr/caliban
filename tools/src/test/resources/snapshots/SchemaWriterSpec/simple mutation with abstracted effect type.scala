import Types._

object Types {
  final case class MutationSetMessageArgs(message: scala.Option[String])

}

object Operations {

  final case class Mutation[F[_]](
    setMessage: MutationSetMessageArgs => F[scala.Option[String]]
  )

}
