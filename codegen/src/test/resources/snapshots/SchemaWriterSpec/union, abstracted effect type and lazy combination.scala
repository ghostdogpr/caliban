import Types._

object Types {
  final case class BLazyFieldWithArgsArgs(int: scala.Option[Int])
  final case class A[F[_]](lazyField: F[scala.Option[Int]])                                   extends U0[F]
  final case class B[F[_]](lazyFieldWithArgs: BLazyFieldWithArgsArgs => F[scala.Option[Int]]) extends U0[F]
  final case class C[F[_]](field: scala.Option[Int])                                          extends U0[F] with U1
  final case class D(field: scala.Option[Int])                                                extends U1

  sealed trait U0[F[_]] extends scala.Product with scala.Serializable
  sealed trait U1       extends scala.Product with scala.Serializable

}

object Operations {

  final case class Query[F[_]](
    effectful: F[U0[F]],
    pure: F[U1]
  )

}
