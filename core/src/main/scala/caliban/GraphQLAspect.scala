package caliban

/**
 * A `GraphQLAspect` is wrapping type similar to a polymorphic function, which is capable
 * of transforming a GraphQL into another while possibly enlarging the required environment type.
 * It allows a flexible way to augment an existing GraphQL with new capabilities or features.
 */
trait GraphQLAspect[+LowerR, -UpperR] { self =>
  def apply[R >: LowerR <: UpperR](gql: GraphQL[R]): GraphQL[R]

  def @@[LowerR1 >: LowerR, UpperR1 <: UpperR](
    other: GraphQLAspect[LowerR1, UpperR1]
  ): GraphQLAspect[LowerR1, UpperR1] =
    new GraphQLAspect[LowerR1, UpperR1] {
      def apply[R >: LowerR1 <: UpperR1](gql: GraphQL[R]): GraphQL[R] =
        other(self(gql))
    }
}
