package caliban

import caliban.CalibanError.ValidationError
import caliban.Rendering.renderTypes
import caliban.execution.{ ExecutionRequest, Executor, QueryExecution }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.{ Document, OperationType }
import caliban.parsing.{ Parser, SourceMapper }
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
  protected val additionalDirectives: List[__Directive]

  private[caliban] def validateRootSchema: IO[ValidationError, RootSchema[R]] =
    Validator.validateSchema(schemaBuilder)

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
   * Converts the schema to a Document.
   */
  final def toDocument: Document =
    Document(
      SchemaDefinition(
        Nil,
        schemaBuilder.query.flatMap(_.opType.name),
        schemaBuilder.mutation.flatMap(_.opType.name),
        schemaBuilder.subscription.flatMap(_.opType.name)
      ) :: schemaBuilder.types.flatMap(_.toTypeDefinition) ++ additionalDirectives.map(_.toDirectiveDefinition),
      SourceMapper.empty
    )

  /**
   * Creates an interpreter from your API. A GraphQLInterpreter is a wrapper around your API that allows
   * adding some middleware around the query execution.
   * Fails with a [[caliban.CalibanError.ValidationError]] if the schema is invalid.
   */
  final def interpreter: IO[ValidationError, GraphQLInterpreter[R, CalibanError]] =
    validateRootSchema.map { schema =>
      lazy val rootType =
        RootType(
          schema.query.opType,
          schema.mutation.map(_.opType),
          schema.subscription.map(_.opType),
          additionalDirectives
        )

      val introWrappers                               = wrappers.collect { case w: IntrospectionWrapper[R] => w }
      lazy val introspectionRootSchema: RootSchema[R] = Introspector.introspect(rootType, introWrappers)
      lazy val introspectionRootType: RootType        = RootType(introspectionRootSchema.query.opType, None, None)

      new GraphQLInterpreter[R, CalibanError] {
        override def check(query: String): IO[CalibanError, Unit] =
          for {
            document      <- Parser.parseQuery(query)
            intro          = Introspector.isIntrospection(document)
            typeToValidate = if (intro) introspectionRootType else rootType
            _             <- Validator.validate(document, typeToValidate)
          } yield ()

        override def executeRequest(
          request: GraphQLRequest,
          skipValidation: Boolean,
          enableIntrospection: Boolean,
          queryExecution: QueryExecution
        ): URIO[R, GraphQLResponse[CalibanError]] =
          decompose(wrappers).flatMap {
            case (overallWrappers, parsingWrappers, validationWrappers, executionWrappers, fieldWrappers, _) =>
              wrap((request: GraphQLRequest) =>
                (for {
                  doc              <- wrap(Parser.parseQuery)(parsingWrappers, request.query.getOrElse(""))
                  intro             = Introspector.isIntrospection(doc)
                  _                <- IO.when(intro && !enableIntrospection) {
                                        IO.fail(CalibanError.ValidationError("Introspection is disabled", ""))
                                      }
                  typeToValidate    = if (intro) introspectionRootType else rootType
                  schemaToExecute   = if (intro) introspectionRootSchema else schema
                  validate          = (doc: Document) =>
                                        Validator
                                          .prepare(
                                            doc,
                                            typeToValidate,
                                            schemaToExecute,
                                            request.operationName,
                                            request.variables.getOrElse(Map()),
                                            skipValidation
                                          )
                  executionRequest <- wrap(validate)(validationWrappers, doc)
                  op                = executionRequest.operationType match {
                                        case OperationType.Query        => schemaToExecute.query
                                        case OperationType.Mutation     => schemaToExecute.mutation.getOrElse(schemaToExecute.query)
                                        case OperationType.Subscription =>
                                          schemaToExecute.subscription.getOrElse(schemaToExecute.query)
                                      }
                  execute           =
                    (req: ExecutionRequest) =>
                      Executor
                        .executeRequest(req, op.plan, request.variables.getOrElse(Map()), fieldWrappers, queryExecution)
                  result           <- wrap(execute)(executionWrappers, executionRequest)
                } yield result).catchAll(Executor.fail)
              )(overallWrappers, request)
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
      override val schemaBuilder: RootSchemaBuilder[R2]    = self.schemaBuilder
      override val wrappers: List[Wrapper[R2]]             = wrapper :: self.wrappers
      override val additionalDirectives: List[__Directive] = self.additionalDirectives
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
      override val schemaBuilder: RootSchemaBuilder[R1]              = self.schemaBuilder |+| that.schemaBuilder
      override protected val wrappers: List[Wrapper[R1]]             = self.wrappers ++ that.wrappers
      override protected val additionalDirectives: List[__Directive] =
        self.additionalDirectives ++ that.additionalDirectives
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
    override protected val schemaBuilder: RootSchemaBuilder[R]     = self.schemaBuilder.copy(
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
    override protected val wrappers: List[Wrapper[R]]              = self.wrappers
    override protected val additionalDirectives: List[__Directive] = self.additionalDirectives
  }

  /**
   * Adds linking to additional types which are unreachable from the root query.
   *
   * @note This is for advanced usage only i.e. when declaring federation type links
   * @param types The type definitions to add.
   */
  final def withAdditionalTypes(types: List[__Type]): GraphQL[R] = new GraphQL[R] {
    override protected val schemaBuilder: RootSchemaBuilder[R]     = self.schemaBuilder.copy(additionalTypes = types)
    override protected val wrappers: List[Wrapper[R]]              = self.wrappers
    override protected val additionalDirectives: List[__Directive] = self.additionalDirectives
  }
}

object GraphQL {

  /**
   * Builds a GraphQL API for the given resolver.
   *
   * It requires an instance of [[caliban.schema.Schema]] for each operation type.
   * This schema will be derived by Magnolia automatically.
   */
  def graphQL[R, Q, M, S: SubscriptionSchema](resolver: RootResolver[Q, M, S], directives: List[__Directive] = Nil)(
    implicit
    querySchema: Schema[R, Q],
    mutationSchema: Schema[R, M],
    subscriptionSchema: Schema[R, S]
  ): GraphQL[R] = new GraphQL[R] {
    val schemaBuilder: RootSchemaBuilder[R]     = RootSchemaBuilder(
      resolver.queryResolver.map(r => Operation(querySchema.toType_(), querySchema.resolve(r))),
      resolver.mutationResolver.map(r => Operation(mutationSchema.toType_(), mutationSchema.resolve(r))),
      resolver.subscriptionResolver.map(r =>
        Operation(subscriptionSchema.toType_(isSubscription = true), subscriptionSchema.resolve(r))
      )
    )
    val wrappers: List[Wrapper[R]]              = Nil
    val additionalDirectives: List[__Directive] = directives
  }
}
