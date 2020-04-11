package caliban.federation

import zio.ZIO
import zquery.ZQuery

trait QueryLift[-R, E, A] {
  def toQuery: ZQuery[R, E, A]
}

object QueryLift {

  implicit def effectToQuery[R, E, A](zio: ZIO[R, E, A]): QueryLift[R, E, A] =
    new QueryLift[R, E, A] {
      override def toQuery: ZQuery[R, E, A] = ZQuery.fromEffect(zio)
    }
}
