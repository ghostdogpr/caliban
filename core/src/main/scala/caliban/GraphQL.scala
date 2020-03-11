package caliban

import caliban.CalibanError.ValidationError
import caliban.Rendering.renderTypes
import caliban.execution.Executor
import caliban.introspection.Introspector
import caliban.parsing.Parser
import caliban.parsing.adt.OperationType
import caliban.schema._
import caliban.validation.Validator
import caliban.wrappers.Wrapper
import caliban.wrappers.Wrapper._
import zio.{ IO, URIO }

/**
 * A `GraphQL[-R]` represents a GraphQL API whose execution requires a ZIO environment of type `R`.
 *
 * It is intended to be created only once, typically when you start your server.
 * The introspection schema will be generated when this class is instantiated.
 */
trait GraphQL[-R] { self =>

  protected val schemaBuilder: RootSchemaBuilder[R]
  protected val wrappers: List[Wrapper[R]]

  /**
   * Returns a string that renders the API types into the GraphQL format.
   */
  final def render: String =
    s"""schema {
       |${schemaBuilder.query.flatMap(_.opType.name).fold("")(n => s"  query: $n\n")}${schemaBuilder.mutation
         .flatMap(_.opType.name)
         .fold("")(n => s"  mutation: $n\n")}${schemaBuilder.subscription
         .flatMap(_.opType.name)
         .fold("")(n => s"  subscription: $n\n")}}
       |
       |${renderTypes(schemaBuilder.types)}""".stripMargin

  /**
   * Creates an interpreter from your API. A GraphQLInterpreter is a wrapper around your API that allows
   * adding some middleware around the query execution.
   * Fails with a [[caliban.CalibanError.ValidationError]] if the schema is invalid.
   */
  final def interpreter: IO[ValidationError, GraphQLInterpreter[R, CalibanError]] =
    Validator
      .validateSchema(schemaBuilder)
      .map { schema =>
        lazy val rootType =
          RootType(schema.query.opType, schema.mutation.map(_.opType), schema.subscription.map(_.opType))
        lazy val introspectionRootSchema: RootSchema[Any] = Introspector.introspect(rootType)
        lazy val introspectionRootType: RootType          = RootType(introspectionRootSchema.query.opType, None, None)

        new GraphQLInterpreter[R, CalibanError] {
          override def check(query: String): IO[CalibanError, Unit] =
            for {
              document       <- Parser.parseQuery(query)
              intro          = Introspector.isIntrospection(document)
              typeToValidate = if (intro) introspectionRootType else rootType
              _              <- Validator.validate(document, typeToValidate)
            } yield ()

          override def execute(
            query: String,
            operationName: Option[String],
            variables: Map[String, InputValue],
            skipValidation: Boolean
          ): URIO[R, GraphQLResponse[CalibanError]] =
            decompose(wrappers).flatMap {
              case (overallWrappers, parsingWrappers, validationWrappers, executionWrappers, fieldWrappers) =>
                wrap((for {
                  doc             <- wrap(Parser.parseQuery(query))(parsingWrappers, query)
                  intro           = Introspector.isIntrospection(doc)
                  typeToValidate  = if (intro) introspectionRootType else rootType
                  schemaToExecute = if (intro) introspectionRootSchema else schema
                  validate = Validator
                    .prepare(doc, typeToValidate, schemaToExecute, operationName, variables, skipValidation)
                  request <- wrap(validate)(validationWrappers, doc)
                  op = request.operationType match {
                    case OperationType.Query        => schemaToExecute.query
                    case OperationType.Mutation     => schemaToExecute.mutation.getOrElse(schemaToExecute.query)
                    case OperationType.Subscription => schemaToExecute.subscription.getOrElse(schemaToExecute.query)
                  }
                  execute = Executor.executeRequest(request, op.plan, variables, fieldWrappers)
                  result  <- wrap(execute)(executionWrappers, request)
                } yield result).catchAll(Executor.fail))(overallWrappers, query)
            }
        }
      }

  /**
   * Attaches a function that will wrap one of the stages of query processing
   * (parsing, validation, execution, field execution or overall).
   * @param wrapper a wrapping function
   * @return a new GraphQL API
   */
  final def withWrapper[R2 <: R](wrapper: Wrapper[R2]): GraphQL[R2] =
    new GraphQL[R2] {
      override val schemaBuilder: RootSchemaBuilder[R2] = self.schemaBuilder
      override val wrappers: List[Wrapper[R2]]          = wrapper :: self.wrappers
    }

  /**
   * A symbolic alias for `withWrapper`.
   */
  final def @@[R2 <: R](wrapper: Wrapper[R2]): GraphQL[R2] = withWrapper(wrapper)

  /**
   * Merges this GraphQL API with another GraphQL API.
   * In case of conflicts (same field declared on both APIs), fields from `that` API will be used.
   * @param that another GraphQL API object
   * @return a new GraphQL API
   */
  final def combine[R1 <: R](that: GraphQL[R1]): GraphQL[R1] =
    new GraphQL[R1] {
      override val schemaBuilder: RootSchemaBuilder[R1]  = self.schemaBuilder |+| that.schemaBuilder
      override protected val wrappers: List[Wrapper[R1]] = self.wrappers ++ that.wrappers
    }

  /**
   * Operator alias for `combine`.
   */
  final def |+|[R1 <: R](that: GraphQL[R1]): GraphQL[R1] = combine(that)

  /**
   * Renames the root queries, mutations and subscriptions objects.
   * @param queriesName a new name for the root queries object
   * @param mutationsName a new name for the root mutations object
   * @param subscriptionsName a new name for the root subscriptions object
   * @return a new GraphQL API
   */
  final def rename(
    queriesName: Option[String] = None,
    mutationsName: Option[String] = None,
    subscriptionsName: Option[String] = None
  ): GraphQL[R] = new GraphQL[R] {
    override protected val schemaBuilder: RootSchemaBuilder[R] = self.schemaBuilder.copy(
      query = queriesName.fold(self.schemaBuilder.query)(name =>
        self.schemaBuilder.query.map(m => m.copy(opType = m.opType.copy(name = Some(name))))
      ),
      mutation = mutationsName.fold(self.schemaBuilder.mutation)(name =>
        self.schemaBuilder.mutation.map(m => m.copy(opType = m.opType.copy(name = Some(name))))
      ),
      subscription = subscriptionsName.fold(self.schemaBuilder.subscription)(name =>
        self.schemaBuilder.subscription.map(m => m.copy(opType = m.opType.copy(name = Some(name))))
      )
    )
    override protected val wrappers: List[Wrapper[R]] = self.wrappers
  }
}

object GraphQL {

  /**
   * Builds a GraphQL API for the given resolver.
   *
   * It requires an instance of [[caliban.schema.Schema]] for each operation type.
   * This schema will be derived by Magnolia automatically.
   */
  def graphQL[R, Q, M, S: SubscriptionSchema](resolver: RootResolver[Q, M, S])(
    implicit querySchema: Schema[R, Q],
    mutationSchema: Schema[R, M],
    subscriptionSchema: Schema[R, S]
  ): GraphQL[R] = new GraphQL[R] {
    val schemaBuilder: RootSchemaBuilder[R] = RootSchemaBuilder(
      resolver.queryResolver.map(r => Operation(querySchema.toType(), querySchema.resolve(r))),
      resolver.mutationResolver.map(r => Operation(mutationSchema.toType(), mutationSchema.resolve(r))),
      resolver.subscriptionResolver.map(r => Operation(subscriptionSchema.toType(), subscriptionSchema.resolve(r)))
    )
    val wrappers: List[Wrapper[R]] = Nil
  }
}
