package caliban.gateway

import caliban.gateway.internal.composition.{ ComposedGraph, SchemaComposer, SupergraphDecomposition }
import caliban.parsing.{ Parser, SourceMapper }
import caliban.parsing.adt.{ Document, OperationType }
import caliban.rendering.DocumentRenderer
import caliban.tools.RemoteSchema
import scala.collection.compat._
import zio._
import zio.test._

import scala.io.Source

/**
 * Composing the projections of `supergraph.graphql` must land in the same place as composing the
 * two subgraphs it was composed from. Schema shape alone does not prove that: a field assigned to
 * the wrong graph produces an identical schema with different routing, so the route maps are the
 * assertion that actually catches an inverted ownership rule.
 *
 * The subgraph SDL is regenerated with rover; see `README.md` next to the fixtures.
 */
object SupergraphRoundTripSpec extends ZIOSpecDefault {

  private def resource(name: String): UIO[String] =
    ZIO
      .scoped(ZIO.fromAutoCloseable(ZIO.attempt(Source.fromResource(s"supergraph/$name"))).map(_.mkString))
      .orDie

  private def parse(name: String, sdl: String): UIO[Document] =
    ZIO.fromEither(Parser.parseQuery(sdl)).orDie

  /**
   * Mirrors `Gateway.load`: the federation flag is derived from the document rather than assumed,
   * and `promoteOrphans` follows it. That is what makes the synthesized `@link` load-bearing — an
   * unlinked projection composes as an ordinary graph, and `Character` (unreachable from the
   * `episodes` `Query`) would silently lose its entity lookup.
   */
  private def prepare(name: String, document: Document): Either[List[String], PreparedSubgraph] = {
    val federation = SchemaComposer.isFederation(document)
    for {
      // Normalizing rather than only building a root type is load-bearing: it folds `extend schema`
      // into the schema definition, which is where composition reads `@link` from. A projection
      // declares its schema outright, so only the checked-in originals depend on the merge.
      normalized <- RemoteSchema
                      .normalize(document, promoteOrphans = federation)
                      .left
                      .map(error => List(s"[$name] ${error.getMessage}"))
      prepared   <- Gateway
                      .prepareSubgraph(
                        Subgraph.federation(name, GatewayTestSupport.unreachableEndpoint, document),
                        normalized.rootType,
                        normalized.document,
                        document,
                        federation
                      )
                      .left
                      .map(SubgraphError(name, _).diagnostics)
    } yield prepared
  }

  private def composeAll(documents: List[(String, Document)]): Either[List[String], ComposedGraph] =
    documents
      .foldRight(Right(Nil): Either[List[String], List[PreparedSubgraph]]) { case ((name, document), result) =>
        result.flatMap(tail => prepare(name, document).map(_ :: tail))
      }
      .flatMap(SchemaComposer.compose(_))

  /** The two checked-in subgraphs, composed exactly as a hand-listed gateway would compose them. */
  private val fromOriginals: UIO[ComposedGraph] =
    for {
      characters <- resource("characters.graphql").flatMap(parse("characters", _))
      episodes   <- resource("episodes.graphql").flatMap(parse("episodes", _))
      composed   <- orDie(composeAll(List("characters" -> characters, "episodes" -> episodes)))
    } yield composed

  /** The same graph reached by decomposing the supergraph and composing the projections. */
  private val fromSupergraph: UIO[ComposedGraph] =
    for {
      sdl       <- resource("supergraph.graphql")
      document  <- parse("supergraph", sdl)
      projected <- orDie(SupergraphDecomposition.decompose(document))
      composed  <- orDie(composeAll(projected.map(entry => entry.graph.name -> entry.document)))
    } yield composed

  private def orDie[A](result: Either[List[String], A]): UIO[A] =
    ZIO.fromEither(result).orDieWith(errors => new AssertionError(errors.mkString("\n")))

  /** Every composed type as SDL, name-ordered, so the comparison is stable and readable on failure. */
  private def render(graph: ComposedGraph): String =
    graph.rootType.types.toList
      .sortBy(_._1)
      .flatMap { case (_, tpe) => tpe.toTypeDefinition }
      .map(definition => DocumentRenderer.render(Document(List(definition), SourceMapper.empty)))
      .mkString("\n")

  def spec = suite("SupergraphRoundTripSpec")(
    test("decomposing then composing yields the same schema as composing the originals") {
      for {
        supergraph <- fromSupergraph
        originals  <- fromOriginals
      } yield assertTrue(render(supergraph) == render(originals))
    },
    test("decomposing then composing yields the same routing as composing the originals") {
      // Schema equality above cannot see ownership: this is the assertion that fails if the field
      // inclusion rule is inverted.
      def sorted[K](routes: Map[K, List[ComposedGraph.FieldRoute]]) = routes.view.mapValues(_.sortBy(_.source)).toMap

      for {
        supergraph <- fromSupergraph
        originals  <- fromOriginals
      } yield assertTrue(
        sorted(supergraph.routes.view.mapValues(_.providers).toMap) == sorted(
          originals.routes.view.mapValues(_.providers).toMap
        ),
        sorted(supergraph.fieldRoutes) == sorted(originals.fieldRoutes)
      )
    },
    test("routes each root field to the graph that declared it") {
      fromSupergraph.map { graph =>
        def route(field: String) = graph.routes.get(OperationType.Query -> field).map(_.providers.map(_.source))

        assertTrue(
          route("characters").contains(List("characters")),
          route("character").contains(List("characters")),
          route("episode").contains(List("episodes")),
          route("episodes").contains(List("episodes"))
        )
      }
    },
    test("resolves an entity field through the graph that owns it, not the one that can reach it") {
      // `Character.isCaptain` is owned by `episodes`, but `Episode.characters` is `characters`-only,
      // so `Character` is unreachable from the `episodes` `Query`. It survives composition only
      // through orphan promotion, which is enabled only when the synthesized federation @link is
      // present — this is the tightest coupling between the link and entity routing.
      fromSupergraph.map { graph =>
        assertTrue(
          graph.fieldRoutes.get("Character" -> "isCaptain").contains(List(ComposedGraph.FieldRoute("episodes"))),
          graph.fieldRoutes.get("Character" -> "name").contains(List(ComposedGraph.FieldRoute("characters"))),
          graph.fieldRoutes.get("Character" -> "biography").contains(List(ComposedGraph.FieldRoute("characters")))
        )
      }
    },
    test("keeps a field owned by both graphs resolvable from both") {
      // `Episode.season` and `Episode.episode` carry no `@join__field`, so they belong to every
      // member graph. Reaching this at all requires the synthesized `@shareable`.
      fromSupergraph.map { graph =>
        assertTrue(
          graph.fieldRoutes
            .get("Episode" -> "season")
            .map(_.map(_.source).sorted)
            .contains(List("characters", "episodes")),
          graph.fieldRoutes
            .get("Episode" -> "episode")
            .map(_.map(_.source).sorted)
            .contains(List("characters", "episodes")),
          graph.fieldRoutes.get("Episode" -> "name").contains(List(ComposedGraph.FieldRoute("episodes"))),
          graph.fieldRoutes.get("Episode" -> "leader").contains(List(ComposedGraph.FieldRoute("characters")))
        )
      }
    }
  )
}
