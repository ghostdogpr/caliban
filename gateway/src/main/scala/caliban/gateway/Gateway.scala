package caliban.gateway

import caliban.gateway.Subgraph.Source
import caliban.gateway.internal._
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, ObjectTypeDefinition }
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.Document
import caliban.tools.RemoteSchema
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import zio._

/**
 * An immutable description of a gateway.
 *
 * A description is reusable: each call to [[build]] creates a new [[GatewayRuntime]] whose resources
 * are owned by the surrounding [[zio.Scope]]. `R` describes the environment required when executing
 * requests; constructing and building the description does not require that environment.
 */
final class Gateway[-R] private[gateway] (
  private val composer: Trace => ZIO[Scope, GatewayBuildError, GatewayRuntime[R]]
) {

  /**
   * Builds an executable runtime within the current scope.
   */
  def build(implicit trace: Trace): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    composer(trace)
}

object Gateway {

  /**
   * Creates a reusable gateway description from one or more subgraphs.
   */
  def compose[R](first: Subgraph[R], rest: Subgraph[R]*): Gateway[R] =
    new Gateway[R](trace => build(first, rest)(trace))

  private def build[R](first: Subgraph[R], rest: Seq[Subgraph[R]])(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] =
    ZIO.scopeWith { parent =>
      for {
        child   <- parent.fork
        // Success commits the child scope to the caller; failure or interruption rolls it back immediately.
        runtime <- child.extend(buildRuntime(first, rest)).onExit {
                     case failure @ Exit.Failure(_) => child.close(failure)
                     case Exit.Success(_)           => ZIO.unit
                   }
      } yield runtime
    }

  private def buildRuntime[R](first: Subgraph[R], rest: Seq[Subgraph[R]])(implicit
    trace: Trace
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] = {
    val subgraphs = first +: rest.toList

    for {
      backend      <-
        if (subgraphs.exists(isRemote))
          HttpClientZioBackend
            .scoped()
            .map(Some(_))
            .mapError(_ => GatewayBuildError("Unable to initialize the remote GraphQL transport."))
        else ZIO.none
      loaded       <- ZIO.foreachPar(subgraphs)(load(_, backend).either)
      contributions = loaded.collect { case Right(contribution) => contribution }
      diagnostics   = nameDiagnostics(subgraphs) ::: loaded.collect { case Left(diagnostic) => diagnostic }
      composed      = SchemaComposition.compose(contributions)
      graph        <- ZIO
                        .fromEither(
                          composed.left.map(errors => GatewayBuildError((diagnostics ::: errors).distinct.sorted))
                        )
                        .flatMap { graph =>
                          if (diagnostics.isEmpty) ZIO.succeed(graph)
                          else ZIO.fail(GatewayBuildError(diagnostics.distinct.sorted))
                        }
      sources      <- ZIO.foreach(subgraphs)(makeSource(_, backend)).map(_.toMap)
    } yield new GatewayRuntimeImpl[R](graph, sources)
  }

  private def load[R](subgraph: Subgraph[R], backend: Option[SttpClient])(implicit
    trace: Trace
  ): IO[String, SchemaContribution] =
    subgraph.source match {
      case Source.Remote(endpoint, schema, federation) =>
        for {
          document    <- RemoteSchemaAcquisition
                           .document(schema, endpoint, federation, backend)
                           .mapError(error => s"[${subgraph.name}] $error")
          rootDocument = ensureFederationQuery(document, federation)
          rootType    <- toRootType(subgraph.name, rootDocument)
        } yield SchemaContribution(subgraph.name, rootType, document, federation, subgraph.lookups)
      case Source.Local(graph)                         =>
        val document   = graph.toDocument
        val federation = SchemaComposition.isFederation(document)
        for {
          rootType <- toRootType(subgraph.name, document)
          _        <- ZIO
                        .fromEither(graph.interpreterEither)
                        .mapError(error => s"[${subgraph.name}] ${error.getMessage}")
        } yield SchemaContribution(subgraph.name, rootType, document, federation, subgraph.lookups)
    }

  private def makeSource[R](subgraph: Subgraph[R], backend: Option[SttpClient])(implicit
    trace: Trace
  ): IO[GatewayBuildError, (String, GraphQLSource[R])] =
    subgraph.source match {
      case Source.Remote(endpoint, _, _) =>
        ZIO
          .fromOption(backend)
          .orElseFail(GatewayBuildError("Unable to initialize the remote GraphQL transport."))
          .map { backend =>
            val source: GraphQLSource[R] = new RemoteGraphQLSource(endpoint, backend)
            subgraph.name -> source
          }
      case Source.Local(graph)           =>
        ZIO
          .fromEither(graph.interpreterEither)
          .mapError(error => GatewayBuildError(s"[${subgraph.name}] ${error.getMessage}"))
          .map(interpreter => subgraph.name -> new LocalGraphQLSource(interpreter))
    }

  private def isRemote[R](subgraph: Subgraph[R]): Boolean =
    subgraph.source match {
      case _: Source.Remote => true
      case _                => false
    }

  private def toRootType(name: String, document: Document): IO[String, caliban.schema.RootType] =
    ZIO
      .fromEither(RemoteSchema.toRootType(document))
      .mapError(error => s"[$name] ${error.getMessage}")

  private def ensureFederationQuery(document: Document, federation: Boolean): Document = {
    val schemaExtensions     = document.typeExtensions.collect { case extension: SchemaExtension => extension }
    val hasDeclaredQuery     =
      document.schemaDefinition.flatMap(_.query).nonEmpty || schemaExtensions.exists(_.query.nonEmpty)
    val hasConventionalQuery =
      document.schemaDefinition.isEmpty && document.objectTypeDefinitions.exists(_.name == "Query")

    if (!federation || hasDeclaredQuery) document
    else if (hasConventionalQuery)
      Document(SchemaDefinition(Nil, Some("Query"), None, None, None) :: document.definitions, document.sourceMapper)
    else {
      val names    = document.typeDefinitions.iterator.map(_.name).toSet
      val rootName = Iterator
        .from(1)
        .map(index => if (index == 1) "CalibanGatewayFederationQuery" else s"CalibanGatewayFederationQuery$index")
        .find(name => !names.contains(name))
        .get
      val root     = ObjectTypeDefinition(
        None,
        rootName,
        Nil,
        Nil,
        List(FieldDefinition(None, "_service", Nil, NamedType("_Service", nonNull = true), Nil))
      )
      val service  =
        if (names.contains("_Service")) Nil
        else
          List(
            ObjectTypeDefinition(
              None,
              "_Service",
              Nil,
              Nil,
              List(FieldDefinition(None, "sdl", Nil, NamedType("String", nonNull = true), Nil))
            )
          )
      val schema   = document.schemaDefinition match {
        case Some(_) =>
          document.definitions.map {
            case definition: SchemaDefinition if definition.query.isEmpty => definition.copy(query = Some(rootName))
            case definition                                               => definition
          }
        case None    => SchemaDefinition(Nil, Some(rootName), None, None, None) :: document.definitions
      }

      Document(schema ::: root :: service, document.sourceMapper)
    }
  }

  private def nameDiagnostics[R](subgraphs: List[Subgraph[R]]): List[String] = {
    val blank     = subgraphs.collect {
      case subgraph if subgraph.name.trim.isEmpty => "[subgraph] Name must not be empty."
    }
    val duplicate = subgraphs
      .groupBy(_.name)
      .collect { case (name, values) if values.size > 1 => s"[subgraph '$name'] Name is used more than once." }
      .toList

    (blank ::: duplicate).sorted
  }
}
