package caliban.federation

import caliban._
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Step.QueryStep
import caliban.schema._
import zio.query.ZQuery

abstract class FederationSupport(
  supportedDirectives: List[__Directive],
  schemaDirectives: List[Directive]
) {
  import FederationHelpers._

  /**
   * Accepts a GraphQL and returns a GraphQL with the minimum settings to support federation. This variant does not
   * provide any stitching capabilities, it merely makes this schema consumable by a graphql federation gateway.
   * @param original The original schema
   * @return A new schema which has been augmented with federation types
   */
  def federate[R](original: GraphQL[R]): GraphQL[R] = {
    import caliban.schema.Schema.auto._

    case class Query(
      _service: _Service,
      _fieldSet: FieldSet = FieldSet("")
    )

    graphQL(
      RootResolver(Query(_service = _Service(original.withSchemaDirectives(schemaDirectives).render))),
      supportedDirectives
    ) |+| original
  }

  def federated[R](resolver: EntityResolver[R], others: EntityResolver[R]*): GraphQLAspect[Nothing, R] =
    new GraphQLAspect[Nothing, R] {
      def apply[R1 <: R](original: GraphQL[R1]): GraphQL[R1] =
        federate(original, resolver, others: _*)
    }

  lazy val federated: GraphQLAspect[Nothing, Any] =
    new GraphQLAspect[Nothing, Any] {
      def apply[R1](original: GraphQL[R1]): GraphQL[R1] =
        federate(original)
    }

  /**
   * Accepts a GraphQL as well as entity resolvers in order to support more advanced federation use cases. This variant
   * will allow the gateway to query for entities by resolver.
   * @param original The original schema
   * @param resolver A type which can resolve a single type by a key which is provided per type using the @key directive
   * @param otherResolvers Additional resolvers to supply
   */
  def federate[R](original: GraphQL[R], resolver: EntityResolver[R], otherResolvers: EntityResolver[R]*): GraphQL[R] = {

    val resolvers = resolver +: otherResolvers.toList

    val genericSchema = new GenericSchema[R] {}
    import genericSchema.auto._

    implicit val entitySchema: Schema[R, _Entity] = new Schema[R, _Entity] {
      override def nullable: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        __Type(
          __TypeKind.UNION,
          name = Some("_Entity"),
          possibleTypes = Some(resolvers.map(_.toType))
        )

      private lazy val _entityMap = resolvers.flatMap(r => r.toType.name.map(_ -> r)).toMap

      /**
       * Resolves `T` by turning a value of type `T` into an execution step that describes how to resolve the value.
       *
       * @param value a value of type `T`
       */
      override def resolve(value: _Entity): Step[R] =
        _entityMap
          .get(value.__typename)
          .fold[Step[R]](Step.NullStep)(resolver => QueryStep(resolver.resolve(value.value)))

    }

    case class Query(
      _entities: RepresentationsArgs => List[_Entity],
      _service: ZQuery[Any, Nothing, _Service],
      _fieldSet: FieldSet = FieldSet("")
    )

    val withSDL = original
      .withAdditionalTypes(resolvers.map(_.toType).flatMap(Types.collectTypes(_)))
      .withSchemaDirectives(schemaDirectives)

    graphQL[R, Query, Unit, Unit](
      RootResolver(
        Query(
          _entities = args => args.representations.map(rep => _Entity(rep.__typename, rep.fields)),
          _service = ZQuery.succeed(_Service(withSDL.render))
        )
      ),
      supportedDirectives
    ) |+| original

  }
}
