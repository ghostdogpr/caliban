package zio.prelude

import zio.prelude.fx.ZPure

package object ops {

  implicit class EnrichZPure(z: ZPure.type) {

    def whenCase[W, S1, R, E, A, B](a: => A)(
      pf: PartialFunction[A, ZPure[W, S1, S1, R, E, B]]
    ): ZPure[W, S1, S1, R, E, Option[B]] =
      ZPure.suspend(pf.lift(a).map(_.map(Some(_))).getOrElse(ZPure.succeed(None)))

    def unless[W, S1, R, E, A](condition: => Boolean)(
      pure: => ZPure[W, S1, S1, R, E, A]
    ): ZPure[W, S1, S1, R, E, Option[A]] =
      when[W, S1, R, E, A](!condition)(pure)

    def when[W, S1, R, E, A](condition: => Boolean)(
      pure: => ZPure[W, S1, S1, R, E, A]
    ): ZPure[W, S1, S1, R, E, Option[A]] =
      ZPure.suspend(if (condition) pure.map(Some(_)) else ZPure.succeed(None))

    def forEachDiscard[W, S1, R, E, A](as: Iterable[A])(
      f: A => ZPure[W, S1, S1, R, E, Any]
    ): ZPure[W, S1, S1, R, E, Unit] =
      ZPure.suspend {
        val iterator = as.iterator
        if (iterator.hasNext) {
          var result: ZPure[W, S1, S1, R, E, Any] = f(iterator.next())
          while (iterator.hasNext)
            result = result *> f(iterator.next())
          result.unit
        } else {
          ZPure.unit
        }
      }

  }

}
