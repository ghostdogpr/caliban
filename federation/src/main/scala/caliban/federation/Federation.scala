package caliban.federation

import caliban.CalibanError.ExecutionError
import caliban.Value.{ NullValue, StringValue }
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Step.QueryStep
import caliban.schema._
import caliban.{ CalibanError, GraphQL, InputValue, RootResolver }
import zio.query.ZQuery

trait Federation {
  import Federation._

  object Key {
    def apply(fields: String): Directive =
      Directive("key", Map("fields" -> StringValue(fields)))
  }

  object Provides {
    def apply(fields: String): Directive =
      Directive("provides", Map("fields" -> StringValue(fields)))
  }

  object Requires {
    def apply(fields: String): Directive =
      Directive("requires", Map("fields" -> StringValue(fields)))
  }

  val Extend = Directive("extends")

  val External = Directive("external")

  /**
   * Accepts a GraphQL and returns a GraphQL with the minimum settings to support federation. This variant does not
   * provide any stitching capabilities, it merely makes this schema consumable by a graphql federation gateway.
   * @param original The original schema
   * @return A new schema which has been augmented with federation types
   */
  def federate[R](original: GraphQL[R]): GraphQL[R] = {
    import Schema._

    case class Query(
      _service: _Service,
      _fieldSet: FieldSet = FieldSet("")
    )

    GraphQL.graphQL(RootResolver(Query(_service = _Service(original.render))), federationDirectives) |+| original
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
    import genericSchema._

    implicit val entitySchema: Schema[R, _Entity] = new Schema[R, _Entity] {
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
        QueryStep(
          _entityMap
            .get(value.__typename)
            .fold[ZQuery[R, CalibanError, Step[R]]](ZQuery.succeed(Step.NullStep))(_.resolve(value.value))
        )
    }

    case class Query(
      _entities: RepresentationsArgs => List[_Entity],
      _service: ZQuery[Any, Nothing, _Service],
      _fieldSet: FieldSet = FieldSet("")
    )

    case class EmptyQuery(
      _service: _Service,
      _fieldSet: FieldSet = FieldSet("")
    )

    val withSDL = original.withAdditionalTypes(resolvers.map(_.toType).flatMap(Types.collectTypes(_)))

    GraphQL.graphQL(
      RootResolver(
        Query(
          _entities = args => args.representations.map(rep => _Entity(rep.__typename, rep.fields)),
          _service = ZQuery.succeed(_Service(withSDL.render))
        )
      ),
      federationDirectives
    ) |+| original

  }
}

object Federation {
  def traverseEither[A, B](list: List[Either[A, B]]): Either[A, List[B]] = {
    val iterator = list.iterator
    val result   = List.newBuilder[B]
    var error    = Option.empty[A]

    while (error.isEmpty && iterator.hasNext) {
      val b = iterator.next()
      b match {
        case Left(value)  =>
          result.clear()
          error = Some(value)
        case Right(value) => result += value
      }
    }

    error.toLeft(result.result())
  }

  private[federation] val _FieldSet = __InputValue(
    "fields",
    None,
    () => Types.makeScalar("_FieldSet"),
    None,
    None
  )

  private[federation] val federationDirectives = List(
    __Directive(
      "external",
      Some("The @external directive is used to mark a field as owned by another service"),
      locations = Set(__DirectiveLocation.FIELD_DEFINITION),
      args = Nil
    ),
    __Directive("requires", None, locations = Set(__DirectiveLocation.FIELD_DEFINITION), args = _FieldSet :: Nil),
    __Directive("provides", None, locations = Set(__DirectiveLocation.FIELD_DEFINITION), args = _FieldSet :: Nil),
    __Directive(
      "key",
      None,
      locations = Set(__DirectiveLocation.OBJECT, __DirectiveLocation.INTERFACE),
      args = _FieldSet :: Nil
    ),
    __Directive("extends", None, locations = Set(__DirectiveLocation.OBJECT, __DirectiveLocation.INTERFACE), Nil)
  )

  case class _Any(__typename: String, fields: InputValue)

  implicit val anySchema: Schema[Any, _Any] =
    Schema.scalarSchema("_Any", None, _ => NullValue)

  val anyArgBuilder: ArgBuilder[_Any] = {
    case v @ InputValue.ObjectValue(fields) =>
      fields
        .get("__typename")
        .collect { case StringValue(__typename) =>
          _Any(__typename, v)
        }
        .toRight(ExecutionError("_Any must contain a __typename value"))
    case other                              => Left(ExecutionError(s"Can't build a _Any from input $other"))
  }

  case class RepresentationsArgs(representations: List[_Any])

  implicit val representationsArgBuilder: ArgBuilder[RepresentationsArgs] = {
    case InputValue.ObjectValue(fields) =>
      fields.get("representations").toRight(ExecutionError("_Any must contain a __typename value")).flatMap {
        case InputValue.ListValue(values) =>
          traverseEither(values.map(anyArgBuilder.build)).map(RepresentationsArgs)
        case other                        => Left(ExecutionError(s"Can't build a representations from input $other"))
      }
    case other                          => Left(ExecutionError(s"Can't build a representations from input $other"))

  }

  case class _Entity(__typename: String, value: InputValue)

  case class FieldSet(fields: String)
  case class _Service(sdl: String)

  implicit val fieldSetSchema: Schema[Any, FieldSet] = Schema.scalarSchema[FieldSet](
    "_FieldSet",
    None,
    fs => StringValue(fs.fields)
  )
}
