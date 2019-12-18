package caliban

import caliban.Rendering.renderTypes
import caliban.execution.Executor
import caliban.execution.QueryAnalyzer.QueryAnalyzer
import caliban.introspection.Introspector
import caliban.parsing.Parser
import caliban.schema.RootSchema.Operation
import caliban.schema._
import caliban.validation.Validator
import zio.{ IO, URIO }

/**
 * A `GraphQL[-R]` represents a GraphQL API whose execution requires a ZIO environment of type `R`.
 *
 * It is intended to be created only once, typically when you start your server.
 * The introspection schema will be generated when this class is instantiated.
 */
trait GraphQL[-R] { self =>

  protected val schema: RootSchema[R]
  protected val queryAnalyzers: List[QueryAnalyzer[R]]

  private lazy val rootType: RootType =
    RootType(schema.query.opType, schema.mutation.map(_.opType), schema.subscription.map(_.opType))
  private lazy val introspectionRootSchema: RootSchema[Any] = Introspector.introspect(rootType)
  private lazy val introspectionRootType: RootType          = RootType(introspectionRootSchema.query.opType, None, None)

  private final def execute(
    query: String,
    operationName: Option[String],
    variables: Map[String, InputValue],
    skipValidation: Boolean
  ): URIO[R, GraphQLResponse[CalibanError]] = {

    val prepare = for {
      document        <- Parser.parseQuery(query)
      intro           = Introspector.isIntrospection(document)
      typeToValidate  = if (intro) introspectionRootType else rootType
      schemaToExecute = if (intro) introspectionRootSchema else schema
      _               <- IO.when(!skipValidation)(Validator.validate(document, typeToValidate))
    } yield (document, schemaToExecute)

    prepare.foldM(
      Executor.fail,
      req => Executor.executeRequest(req._1, req._2, operationName, variables, queryAnalyzers)
    )
  }

  /**
   * Parses and validates the provided query against this API.
   * @param query a string containing the GraphQL query.
   * @return an effect that either fails with a [[CalibanError]] or succeeds with `Unit`
   */
  final def check(query: String): IO[CalibanError, Unit] =
    for {
      document       <- Parser.parseQuery(query)
      intro          = Introspector.isIntrospection(document)
      typeToValidate = if (intro) introspectionRootType else rootType
      _              <- Validator.validate(document, typeToValidate)
    } yield ()

  /**
   * Returns a string that renders the API types into the GraphQL format.
   */
  final def render: String = renderTypes(rootType.types)

  /**
   * Creates an interpreter from your API. A GraphQLInterpreter is a wrapper around your API that allows
   * adding some middleware around the query execution.
   */
  final def interpreter: GraphQLInterpreter[R, CalibanError] =
    (query: String, operationName: Option[String], variables: Map[String, InputValue], skipValidation: Boolean) =>
      self.execute(query, operationName, variables, skipValidation)

  /**
   * Attaches a function that will analyze each query before execution, possibly modify or reject it.
   * @param queryAnalyzer a function from `Field` to `ZIO[R, CalibanError, Field]`
   * @return a new GraphQL API
   */
  final def withQueryAnalyzer[R2 <: R](queryAnalyzer: QueryAnalyzer[R2]): GraphQL[R2] =
    new GraphQL[R2] {
      override val schema: RootSchema[R2]                  = self.schema
      override val queryAnalyzers: List[QueryAnalyzer[R2]] = queryAnalyzer :: self.queryAnalyzers
    }

  /**
   * Merges this GraphQL API with another GraphQL API.
   * In case of conflicts (same field declared on both APIs), fields from the this API will be favored.
   * @param that another GraphQL API object
   * @return a new GraphQL API
   */
  final def |+|[R1 <: R](that: GraphQL[R1]): GraphQL[R1] =
    new GraphQL[R1] {
      override val schema: RootSchema[R1]                  = self.schema |+| that.schema
      override val queryAnalyzers: List[QueryAnalyzer[R1]] = self.queryAnalyzers ++ that.queryAnalyzers
    }

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
    override protected val schema: RootSchema[R] = self.schema.copy(
      query = queriesName.fold(self.schema.query)(
        name => self.schema.query.copy(opType = self.schema.query.opType.copy(name = Some(name)))
      ),
      mutation = mutationsName.fold(self.schema.mutation)(
        name => self.schema.mutation.map(m => m.copy(opType = m.opType.copy(name = Some(name))))
      ),
      subscription = subscriptionsName.fold(self.schema.subscription)(
        name => self.schema.subscription.map(m => m.copy(opType = m.opType.copy(name = Some(name))))
      )
    )
    override protected val queryAnalyzers: List[QueryAnalyzer[R]] = self.queryAnalyzers
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
    val schema: RootSchema[R] = RootSchema(
      Operation(querySchema.toType(), querySchema.resolve(resolver.queryResolver)),
      resolver.mutationResolver.map(r => Operation(mutationSchema.toType(), mutationSchema.resolve(r))),
      resolver.subscriptionResolver.map(r => Operation(subscriptionSchema.toType(), subscriptionSchema.resolve(r)))
    )
    val queryAnalyzers: List[QueryAnalyzer[R]] = Nil
  }
}
