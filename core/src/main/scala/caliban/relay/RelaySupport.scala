package caliban.relay

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.introspection.adt.{ __Field, __Type, __TypeKind, TypeVisitor }
import caliban.schema.Step.NullStep
import caliban.schema.{ ArgBuilder, GenericSchema, Schema, Step }
import caliban.transformers.Transformer
import caliban.{ graphQL, GraphQL, GraphQLAspect, RootResolver }
import zio.NonEmptyChunk
import zio.query.ZQuery

object RelaySupport {

  def withGlobalIdentifiers[ID]: WithGlobalIdentifiers[ID] =
    WithGlobalIdentifiers()

  def globalIdentifiers[R, ID: ArgBuilder](
    original: GraphQL[R],
    resolvers: NonEmptyChunk[NodeResolver[R, ID]]
  )(implicit idSchema: Schema[Any, ID], typeResolver: TypeResolver[ID]): GraphQL[R] = {
    val _resolvers    = resolvers.toList
    lazy val _typeMap = _resolvers.flatMap(r => r.toType.name.map(_ -> r)).toMap

    val genericSchema                                      = new GenericSchema[R] {}
    implicit val nodeArgBuilder: ArgBuilder[NodeArgs[ID]]  = ArgBuilder.gen[NodeArgs[ID]]
    implicit val nodeArgsSchema: Schema[Any, NodeArgs[ID]] = Schema.gen[Any, NodeArgs[ID]]

    val nodeType = __Type(
      __TypeKind.INTERFACE,
      name = Some("Node"),
      possibleTypes = Some(_resolvers.map(_.toType)),
      fields = _ =>
        Some(
          List(
            __Field(
              "id",
              None,
              args = _ => Nil,
              `type` = () => idSchema.toType_(isInput = true).nonNull
            )
          )
        )
    )

    implicit val nodeSchema: Schema[R, Node[ID]] = new Schema[R, Node[ID]] {
      override final val nullable: Boolean                                         = true
      override final def toType(isInput: Boolean, isSubscription: Boolean): __Type = nodeType

      override final def resolve(value: Node[ID]): Step[R] =
        _typeMap.getOrElse(value.__typename, null) match {
          case null     => NullStep
          case resolver => resolver.resolve(value.id)
        }
    }

    val transformer = new Transformer[Any] {
      private def shouldAdd(__type: __Type): Boolean =
        if (__type.innerType.name.isEmpty) false
        else typeNames.contains(__type.name.get) && !__type.implements.contains("Node")

      override val typeVisitor: TypeVisitor =
        TypeVisitor.interfaces.addWith(inner =>
          if (shouldAdd(inner)) List(nodeType)
          else Nil
        )

      override protected lazy val typeNames: collection.Set[String] =
        _typeMap.keySet

      override protected def transformStep[R1 <: Any](step: Step.ObjectStep[R1], field: Field): Step.ObjectStep[R1] =
        step
    }

    case class Query(
      node: NodeArgs[ID] => ZQuery[Any, ExecutionError, Node[ID]]
    )

    implicit val querySchema: Schema[R, Query] = genericSchema.gen[R, Query]

    (original |+| graphQL[R, Query, Unit, Unit](
      RootResolver(
        Query(node =
          args =>
            typeResolver.resolve(args.id) match {
              case Left(error)     => ZQuery.fail(error)
              case Right(typename) => ZQuery.succeed(Node(typename, args.id))
            }
        )
      )
    )).transform(transformer)
  }

  private case class Node[A](__typename: String, id: A)

  private case class NodeArgs[ID](id: ID)

  final case class WithGlobalIdentifiers[ID](dummy: Boolean = false) extends AnyVal {
    def apply[R](resolver: NodeResolver[R, ID], rest: NodeResolver[R, ID]*)(implicit
      argBuilder: ArgBuilder[ID],
      schema: Schema[Any, ID],
      identifiable: TypeResolver[ID]
    ): GraphQLAspect[Nothing, R] =
      new GraphQLAspect[Nothing, R] {
        def apply[R1 <: R](original: GraphQL[R1]): GraphQL[R1] =
          globalIdentifiers[R1, ID](original, NonEmptyChunk(resolver, rest: _*))
      }
  }
}
