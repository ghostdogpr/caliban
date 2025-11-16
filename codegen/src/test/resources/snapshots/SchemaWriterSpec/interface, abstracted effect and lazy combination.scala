import Types._

import caliban.schema.Annotations._

object Types {

  final case class T0[F[_]](i: F[Int])              extends Direct[F]
  final case class T1[F[_]](i: scala.Option[T2[F]]) extends Transitive[F]
  final case class T2[F[_]](i: F[scala.Option[Int]])

  @GQLInterface
  sealed trait Direct[F[_]] extends scala.Product with scala.Serializable {
    def i: F[Int]
  }

  @GQLInterface
  sealed trait Transitive[F[_]] extends scala.Product with scala.Serializable {
    def i: scala.Option[T2[F]]
  }

}

object Operations {

  final case class Query[F[_]](
    t0: F[T0[F]],
    t1: F[T1[F]]
  )

}
