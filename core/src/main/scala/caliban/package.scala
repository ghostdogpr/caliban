import caliban.execution.Feature
import caliban.introspection.adt.__Directive
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.{ Directive, Document }
import caliban.rendering.DocumentRenderer
import caliban.schema.Types.collectTypes
import caliban.schema._
import caliban.transformers.Transformer
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
    override protected val schemaBuilder: RootSchemaBuilder[R]     = RootSchemaBuilder(
      resolver.queryResolver.map(r => Operation(querySchema.toType_(), querySchema.resolve(r))),
      resolver.mutationResolver.map(r => Operation(mutationSchema.toType_(), mutationSchema.resolve(r))),
      resolver.subscriptionResolver.map(r =>
        Operation(subscriptionSchema.toType_(isSubscription = true), subscriptionSchema.resolve(r))
      ),
      schemaDirectives = schemaDirectives,
      schemaDescription = schemaDescription
    )
    override protected val wrappers: List[Wrapper[R]]              = Nil
    override protected val additionalDirectives: List[__Directive] = directives
    override protected val features: Set[Feature]                  = Set.empty
    override protected val transformer: Transformer[R]             = Transformer.empty
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

  /**
   * Returns a string that renders the given schema into the GraphQL SDL.
   */
  def renderSchema[Q, M, S](
    directives: List[__Directive] = Nil,
    schemaDirectives: List[Directive] = Nil,
    schemaDescription: Option[String] = None
  )(implicit querySchema: Schema[Any, Q], mutationSchema: Schema[Any, M], subscriptionSchema: Schema[Any, S]): String =
    renderSchemaWith[Any, Q, M, S](directives, schemaDirectives, schemaDescription)

  /**
   * Returns a string that renders the given schema into the GraphQL SDL.
   * This variant of the method allows specifying the environment type when it's not `Any`.
   */
  def renderSchemaWith[R, Q, M, S](
    directives: List[__Directive] = Nil,
    schemaDirectives: List[Directive] = Nil,
    schemaDescription: Option[String] = None
  )(implicit querySchema: Schema[R, Q], mutationSchema: Schema[R, M], subscriptionSchema: Schema[R, S]): String = {
    val hasQuery        = querySchema.toType_().allFields.nonEmpty
    val hasMutation     = mutationSchema.toType_().allFields.nonEmpty
    val hasSubscription = subscriptionSchema.toType_().allFields.nonEmpty
    DocumentRenderer.render(
      Document(
        SchemaDefinition(
          schemaDirectives,
          if (hasQuery) querySchema.toType_().name else None,
          if (hasMutation) mutationSchema.toType_().name else None,
          if (hasSubscription) subscriptionSchema.toType_().name else None,
          schemaDescription
        ) ::
          ((if (hasQuery) collectTypes(querySchema.toType_()).flatMap(_.toTypeDefinition) else Nil)
            ++ (if (hasMutation) collectTypes(mutationSchema.toType_()).flatMap(_.toTypeDefinition) else Nil)
            ++ (if (hasSubscription) collectTypes(subscriptionSchema.toType_()).flatMap(_.toTypeDefinition) else Nil))
            .groupBy(_.name)
            .flatMap(_._2.headOption)
            .toList
          ++ directives.map(_.toDirectiveDefinition),
        SourceMapper.empty
      )
    )
  }
}
