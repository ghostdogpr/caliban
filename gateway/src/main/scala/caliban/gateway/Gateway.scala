package caliban.gateway

import caliban.gateway.internal.{ RemoteGatewayRuntime, RemoteGraphQLSource, SchemaComposition, SchemaContribution }
import caliban.parsing.adt.Definition.TypeSystemDefinition.SchemaDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, ObjectTypeDefinition }
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.Document
import caliban.tools.RemoteSchema
import sttp.client4.httpclient.zio.HttpClientZioBackend
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
  ): ZIO[Scope, GatewayBuildError, GatewayRuntime[R]] = {
    val subgraphs = first +: rest.toList

    for {
      loaded       <- ZIO.foreach(subgraphs)(load(_).either)
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
      backend      <- HttpClientZioBackend
                        .scoped()
                        .mapError(_ => GatewayBuildError("Unable to initialize the remote GraphQL transport."))
      sources       =
        subgraphs.iterator.map(subgraph => subgraph.name -> new RemoteGraphQLSource(subgraph.endpoint, backend)).toMap
    } yield new RemoteGatewayRuntime[R](graph, sources)
  }

  private def load[R](subgraph: Subgraph[R])(implicit trace: Trace): IO[String, SchemaContribution] =
    for {
      document    <- subgraph.schema.document.mapError(error => s"[${subgraph.name}] ${error.getMessage}")
      rootDocument = ensureFederationQuery(document, subgraph.isFederation)
      rootType    <- ZIO
                       .fromEither(RemoteSchema.toRootType(rootDocument))
                       .mapError(error => s"[${subgraph.name}] ${error.getMessage}")
    } yield SchemaContribution(subgraph.name, rootType, document, subgraph.isFederation)

  private def ensureFederationQuery(document: Document, federation: Boolean): Document = {
    val schemaExtensions = document.typeExtensions.collect { case extension: SchemaExtension => extension }
    val hasSchema        = document.schemaDefinition.nonEmpty || schemaExtensions.nonEmpty
    val hasQuery         =
      if (hasSchema)
        document.schemaDefinition.flatMap(_.query).nonEmpty || schemaExtensions.exists(_.query.nonEmpty)
      else document.objectTypeDefinitions.exists(_.name == "Query")

    if (!federation || hasQuery) document
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
