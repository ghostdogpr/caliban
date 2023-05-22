package caliban

import caliban.CalibanError.ValidationError
import caliban.Rendering.{ renderDirectives, renderSchemaDirectives, renderTypes }
import caliban.execution.{ ExecutionRequest, Executor, Feature }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.{ Directive, Document, OperationType }
import caliban.parsing.{ Parser, SourceMapper, VariablesCoercer }
import caliban.schema._
import caliban.validation.Validator
import caliban.wrappers.Wrapper
import caliban.wrappers.Wrapper._
import zio.{ IO, Trace, URIO, ZIO }

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
  protected val features: Set[Feature]

  private[caliban] def validateRootSchema(implicit trace: Trace): IO[ValidationError, RootSchema[R]] =
    ZIO.fromEither(validatedRootSchemaCached)

  private lazy val validatedRootSchemaCached: Either[ValidationError, RootSchema[R]] =
    Validator.validateSchemaEither(schemaBuilder)

  /**
   * Returns a string that renders the API types into the GraphQL format.
   */
  final def render: String = {
    val parts            = Seq(
      schemaBuilder.query.flatMap(_.opType.name).map(n => s"  query: $n"),
      schemaBuilder.mutation.flatMap(_.opType.name).map(n => s"  mutation: $n"),
      schemaBuilder.subscription.flatMap(_.opType.name).map(n => s"  subscription: $n")
    )
    val schemaDirectives = renderSchemaDirectives(schemaBuilder.schemaDirectives)

    val schema = parts.flatten.mkString("\n") match {
      case ""        => ""
      case something => s"""schema $schemaDirectives{
                           |$something
                           |}""".stripMargin
    }

    val directivesPrefix = renderDirectives(additionalDirectives) match {
      case ""           => ""
      case directiveStr => directiveStr + "\n\n"
    }

    s"""$directivesPrefix$schema
       |
       |${renderTypes(schemaBuilder.types)}""".stripMargin
  }

  /**
   * Converts the schema to a Document.
   */
  final def toDocument: Document =
    Document(
      SchemaDefinition(
        schemaBuilder.schemaDirectives,
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
  final def interpreter(implicit trace: Trace): IO[ValidationError, GraphQLInterpreter[R, CalibanError]] = {
    val validated = validatedRootSchemaCached.map { schema =>
      lazy val rootType =
        RootType(
          schema.query.opType,
          schema.mutation.map(_.opType),
          schema.subscription.map(_.opType),
          schemaBuilder.additionalTypes,
          additionalDirectives
        )

      val introWrappers                               = wrappers.collect { case w: IntrospectionWrapper[R] => w }
      lazy val introspectionRootSchema: RootSchema[R] = Introspector.introspect(rootType, introWrappers)
      lazy val introspectionRootType: RootType        = RootType(introspectionRootSchema.query.opType, None, None)

      new GraphQLInterpreter[R, CalibanError] {
        override def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
          for {
            document      <- Parser.parseQuery(query)
            intro          = Introspector.isIntrospection(document)
            typeToValidate = if (intro) introspectionRootType else rootType
            _             <- Validator.validate(document, typeToValidate)
          } yield ()

        override def executeRequest(request: GraphQLRequest)(implicit
          trace: Trace
        ): URIO[R, GraphQLResponse[CalibanError]] =
          decompose(wrappers).flatMap {
            case (overallWrappers, parsingWrappers, validationWrappers, executionWrappers, fieldWrappers, _) =>
              wrap((request: GraphQLRequest) =>
                (for {
                  doc                                  <- wrap(Parser.parseQuery)(parsingWrappers, request.query.getOrElse(""))
                  intro                                 = doc.isIntrospection
                  config                               <- Configurator.configuration.map { config =>
                                                            (config.skipValidation, config.enableIntrospection)
                                                          }
                  (skipValidation, enableIntrospection) = config
                  _                                    <- ZIO.when(intro && !enableIntrospection) {
                                                            ZIO.fail(CalibanError.ValidationError("Introspection is disabled", ""))
                                                          }
                  typeToValidate                        = if (intro) introspectionRootType else rootType
                  schemaToExecute                       = if (intro) introspectionRootSchema else schema
                  validatedReq                         <- VariablesCoercer.coerceVariables(request, doc, typeToValidate, skipValidation)
                  validate                              = (doc: Document) =>
                                                            for {
                                                              config       <- Configurator.configuration
                                                              executionReq <- Validator.prepare(
                                                                                doc,
                                                                                typeToValidate,
                                                                                schemaToExecute,
                                                                                validatedReq.operationName,
                                                                                validatedReq.variables.getOrElse(Map.empty),
                                                                                config.skipValidation,
                                                                                config.validations
                                                                              )
                                                            } yield executionReq
                  executionRequest                     <- wrap(validate)(validationWrappers, doc)
                  op                                    = executionRequest.operationType match {
                                                            case OperationType.Query        => schemaToExecute.query
                                                            case OperationType.Mutation     => schemaToExecute.mutation.getOrElse(schemaToExecute.query)
                                                            case OperationType.Subscription =>
                                                              schemaToExecute.subscription.getOrElse(schemaToExecute.query)
                                                          }
                  execute                               = (req: ExecutionRequest) =>
                                                            for {
                                                              queryExecution <- Configurator.configuration.map(_.queryExecution)
                                                              res            <- Executor.executeRequest(req, op.plan, fieldWrappers, queryExecution, features)
                                                            } yield res
                  result                               <- wrap(execute)(executionWrappers, executionRequest)
                } yield result).catchAll(Executor.fail)
              )(overallWrappers, request)
          }
      }
    }
    ZIO.fromEither(validated)
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
      override val features: Set[Feature]                  = self.features
    }

  /**
   * Attaches an aspect that will wrap the entire GraphQL so that it can be manipulated.
   * This method is a higher-level abstraction of [[withWrapper]] which allows the caller to
   * completely replace or change all aspects of the schema.
   * @param aspect A wrapper type that will be applied to this GraphQL
   * @return A new GraphQL API
   */
  final def @@[LowerR <: UpperR, UpperR <: R](aspect: GraphQLAspect[LowerR, UpperR]): GraphQL[UpperR] =
    aspect(self)

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
      override protected val features: Set[Feature]                  = self.features ++ that.features
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
    override protected val features: Set[Feature]                  = self.features
  }

  /**
   * Adds linking to additional types which are unreachable from the root query.
   *
   * @note This is for advanced usage only i.e. when declaring federation type links
   * @param types The type definitions to add.
   */
  final def withAdditionalTypes(types: List[__Type]): GraphQL[R] = new GraphQL[R] {
    override protected val schemaBuilder: RootSchemaBuilder[R]     =
      self.schemaBuilder.copy(additionalTypes = self.schemaBuilder.additionalTypes ++ types)
    override protected val wrappers: List[Wrapper[R]]              = self.wrappers
    override protected val additionalDirectives: List[__Directive] = self.additionalDirectives
    override protected val features: Set[Feature]                  = self.features
  }

  final def withSchemaDirectives(directives: List[Directive]): GraphQL[R] = new GraphQL[R] {
    override protected val schemaBuilder: RootSchemaBuilder[R]     =
      self.schemaBuilder.copy(schemaDirectives = self.schemaBuilder.schemaDirectives ++ directives)
    override protected val wrappers: List[Wrapper[R]]              = self.wrappers
    override protected val additionalDirectives: List[__Directive] = self.additionalDirectives
    override protected val features: Set[Feature]                  = self.features
  }

  final def withAdditionalDirectives(directives: List[__Directive]): GraphQL[R] = new GraphQL[R] {
    override protected val schemaBuilder: RootSchemaBuilder[R]     = self.schemaBuilder
    override protected val wrappers: List[Wrapper[R]]              = self.wrappers
    override protected val additionalDirectives: List[__Directive] = self.additionalDirectives ++ directives
    override protected val features: Set[Feature]                  = self.features
  }

  final def enable(feature: Feature): GraphQL[R] = new GraphQL[R] {
    override protected val schemaBuilder: RootSchemaBuilder[R]     = self.schemaBuilder
    override protected val wrappers: List[Wrapper[R]]              = self.wrappers
    override protected val additionalDirectives: List[__Directive] = self.additionalDirectives
    override protected val features: Set[Feature]                  = self.features + feature
  }
}

object GraphQL {

  /**
   * Builds a GraphQL API for the given resolver.
   *
   * It requires an instance of [[caliban.schema.Schema]] for each operation type.
   * This schema will be derived by Magnolia automatically.
   */
  @deprecated("Use caliban.graphQL", "2.1.0") def graphQL[R, Q, M, S: SubscriptionSchema](
    resolver: RootResolver[Q, M, S],
    directives: List[__Directive] = Nil,
    schemaDirectives: List[Directive] = Nil
  )(implicit querySchema: Schema[R, Q], mutationSchema: Schema[R, M], subscriptionSchema: Schema[R, S]): GraphQL[R] =
    caliban.graphQL[R, Q, M, S](resolver, directives, schemaDirectives)
}
