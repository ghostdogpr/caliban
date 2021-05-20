package caliban.tools.stitching

import zio._

case class RemoteResolver[-R, +E, -A, +B](
  run: A => ZIO[R, E, B]
) { self =>
  def mapM[R1 <: R, E1 >: E, A1 <: A, C1](bc: B => ZIO[R1, E1, C1]): RemoteResolver[R1, E1, A, C1] =
    RemoteResolver((x: A) => self.run(x).flatMap(bc))

  def map[C](bc: B => C): RemoteResolver[R, E, A, C] = RemoteResolver((x: A) => self.run(x).map(bc))

  def >>>[R1 <: R, E1 >: E, C](
    other: RemoteResolver[R1, E1, B, C]
  ): RemoteResolver[R1, E1, A, C] =
    RemoteResolver((a: A) => self.run(a).flatMap(other.run(_)))
}

object RemoteResolver {
  def fromFunction[A, B](f: A => B): RemoteResolver[Any, Nothing, A, B] = RemoteResolver((a: A) => ZIO.succeed(f(a)))

  def fromFunctionM[A, R, E, B](f: A => ZIO[R, E, B]): RemoteResolver[R, E, A, B] = RemoteResolver((a: A) => f(a))

  def fromEffect[A, R, E, B](effect: ZIO[R, E, B]): RemoteResolver[R, E, A, B] = RemoteResolver((_: Any) => effect)
}
