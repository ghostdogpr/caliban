import caliban.introspection.adt.__Directive
import caliban.parsing.adt.Directive
import caliban.rendering.DocumentRenderer
import caliban.schema.Types.collectTypes
import caliban.schema._
import caliban.wrappers.Wrapper

package object caliban {

  /**
   * Builds a GraphQL API for the given resolver.
   *
   * It requires an instance of [[caliban.schema.Schema]] for each operation type.
   * This schema will be derived by Magnolia automatically.
   */
  def graphQL[R, Q, M, S: SubscriptionSchema](
    resolver: RootResolver[Q, M, S],
    directives: List[__Directive] = Nil,
    schemaDirectives: List[Directive] = Nil,
    schemaDescription: Option[String] = None
  )(implicit
    querySchema: Schema[R, Q],
    mutationSchema: Schema[R, M],
    subscriptionSchema: Schema[R, S]
  ): GraphQL[R] = new GraphQL[R] {
    val schemaBuilder: RootSchemaBuilder[R]     = RootSchemaBuilder(
      resolver.queryResolver.map(r => Operation(querySchema.toType_(), querySchema.resolve(r))),
      resolver.mutationResolver.map(r => Operation(mutationSchema.toType_(), mutationSchema.resolve(r))),
      resolver.subscriptionResolver.map(r =>
        Operation(subscriptionSchema.toType_(isSubscription = true), subscriptionSchema.resolve(r))
      ),
      schemaDirectives = schemaDirectives,
      schemaDescription = schemaDescription
    )
    val wrappers: List[Wrapper[R]]              = Nil
    val additionalDirectives: List[__Directive] = directives
    val features                                = Set.empty
  }

  /**
   * Returns a string that renders the given type into the GraphQL SDL.
   */
  def render[T](implicit schema: Schema[Any, T]): String =
    renderWith[Any, T]

  /**
   * Returns a string that renders the given type into the GraphQL SDL.
   * This variant of the method allows specifying the environment type when it's not `Any`.
   */
  def renderWith[R, T](implicit schema: Schema[R, T]): String =
    DocumentRenderer.typesRenderer.render(collectTypes(schema.toType_(), Nil))
}
