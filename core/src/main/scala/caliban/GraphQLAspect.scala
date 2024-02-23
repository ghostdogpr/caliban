package caliban

import caliban.introspection.adt.{ __Directive, __Type }
import caliban.parsing.adt.Directive
import zio.Trace
import zio.stacktracer.TracingImplicits.disableAutoTrace

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

object GraphQLAspect {

  def withSchemaDirectives(directives: List[Directive]): GraphQLAspect[Nothing, Any] = new GraphQLAspect[Nothing, Any] {
    def apply[R](gql: GraphQL[R]): GraphQL[R] = gql.withSchemaDirectives(directives)
  }

  def withDirectives(directives: List[__Directive]): GraphQLAspect[Nothing, Any] = new GraphQLAspect[Nothing, Any] {
    def apply[R](gql: GraphQL[R]): GraphQL[R] = gql.withAdditionalDirectives(directives)
  }

  def withTypes(types: List[__Type]): GraphQLAspect[Nothing, Any] = new GraphQLAspect[Nothing, Any] {
    def apply[R](gql: GraphQL[R]): GraphQL[R] = gql.withAdditionalTypes(types)
  }

}
