import Types._

object Types {
  final case class MutationSetMessageArgs(message: scala.Option[String])

}

object Operations {

  final case class Mutation(
    setMessage: MutationSetMessageArgs => zio.UIO[scala.Option[String]]
  )

}
