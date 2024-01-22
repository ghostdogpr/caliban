package caliban

import caliban.introspection.adt.__Directive
import caliban.parsing.adt.Directive
import caliban.schema.{ Schema, SchemaDerivation, SubscriptionSchema }

trait CalibanVersionSpecific {
  def graphQL[R, Q, M, S: SubscriptionSchema](
    resolver: RootResolver[Q, M, S],
    directives: List[__Directive] = Nil,
    schemaDirectives: List[Directive] = Nil,
    schemaDescription: Option[String] = None
  )(implicit
    querySchema: Schema[R, Q],
    mutationSchema: Schema[R, M],
    subscriptionSchema: Schema[R, S]
  ): GraphQL[R]

  extension [Q, M, S: SubscriptionSchema](resolver: RootResolver[Q, M, S]) {

    def toGraphQL: GraphQLPartiallyApplied[Q, M, S] =
      new GraphQLPartiallyApplied(resolver)

    def toGraphQL(
      directives: List[__Directive] = Nil,
      schemaDirectives: List[Directive] = Nil,
      schemaDescription: Option[String] = None
    ): GraphQLPartiallyApplied[Q, M, S] =
      new GraphQLPartiallyApplied(resolver, directives, schemaDirectives, schemaDescription)

  }

  final class GraphQLPartiallyApplied[Q, M, S: SubscriptionSchema](
    resolver: RootResolver[Q, M, S],
    directives: List[__Directive] = Nil,
    schemaDirectives: List[Directive] = Nil,
    schemaDescription: Option[String] = None
  ) {
    def apply[R](using
      querySchema: Schema[R, Q],
      mutationSchema: Schema[R, M],
      subscriptionSchema: Schema[R, S]
    ): GraphQL[R] =
      graphQL[R, Q, M, S](resolver, directives, schemaDirectives, schemaDescription)

    def forSchema[R, T <: SchemaDerivation.Tagged[R, T]](using
      querySchema: Schema[R & T, Q],
      mutationSchema: Schema[R & T, M],
      subscriptionSchema: Schema[R & T, S]
    ): GraphQL[R] =
      graphQL[R, Q, M, S](resolver, directives, schemaDirectives, schemaDescription)(
        summon,
        querySchema.asInstanceOf[Schema[R, Q]],
        mutationSchema.asInstanceOf[Schema[R, M]],
        subscriptionSchema.asInstanceOf[Schema[R, S]]
      )
  }

}
