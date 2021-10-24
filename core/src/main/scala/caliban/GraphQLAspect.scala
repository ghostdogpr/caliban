package caliban

import caliban.wrappers.Wrapper

trait GraphQLAspect[-R] { self =>
  def apply[R1 <: R](g: GraphQL[R]): GraphQL[R1]
  def @@[R1 <: R](other: GraphQL[R1]): GraphQLAspect[R1] = new GraphQLAspect[R1] {
      def apply[R2 <: R1](g: GraphQL[R2]): GraphQL[R2] = other(self(g))
  }
}

object GraphQLAspect {
  case class WrappingAspect[-R](wrapper: Wrapper[R]) extends GraphQLAspect[R] {
    def apply[R1 <: R](g: GraphQL[R]): GraphQL[R1] = g @@ wrapper
  }
}
