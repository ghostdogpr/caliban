package caliban

import zio.*

extension [R](gql: GraphQL[R]) {

  def quick: IO[CalibanError.ValidationError, QuickAdapter[R, CalibanError]] =
    gql.interpreter.map(new QuickAdapter(_))

}
