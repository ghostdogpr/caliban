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
 * `context-supergraph.graphql` is the same test for `@context`/`@fromContext`, which the composer
 * does not merely relabel: it deletes each context argument from the field it belongs to and
 * namespaces every context name by the subgraph that declared it. Nothing but composing the
 * projections proves the argument came back in a shape `SchemaComposer` still accepts.
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

  /** The checked-in subgraphs, composed exactly as a hand-listed gateway would compose them. */
  private def fromOriginals(names: (String, String)*): UIO[ComposedGraph] =
    for {
      documents <- ZIO.foreach(names.toList) { case (name, file) =>
                     resource(file).flatMap(parse(name, _)).map(name -> _)
                   }
      composed  <- orDie(composeAll(documents))
    } yield composed

  /** The same graph reached by decomposing the supergraph and composing the projections. */
  private def fromSupergraph(file: String): UIO[ComposedGraph] =
    for {
      sdl       <- resource(file)
      document  <- parse("supergraph", sdl)
      projected <- orDie(SupergraphDecomposition.decompose(document))
      composed  <- orDie(composeAll(projected.map(entry => entry.graph.name -> entry.document)))
    } yield composed

  private val characterGraphFromSupergraph: UIO[ComposedGraph] = fromSupergraph("supergraph.graphql")

  private val characterGraphFromOriginals: UIO[ComposedGraph] =
    fromOriginals("characters" -> "characters.graphql", "episodes" -> "episodes.graphql")

  /** The `@context`/`@fromContext` fixture, whose whole point is that the metadata survives a round trip. */
  private val contextGraphFromSupergraph: UIO[ComposedGraph] = fromSupergraph("context-supergraph.graphql")

  private val contextGraphFromOriginals: UIO[ComposedGraph] =
    fromOriginals("characters" -> "context-characters.graphql", "episodes" -> "context-episodes.graphql")

  /**
   * Wraps a supergraph body in the links and join graph enum a projection needs, for a case no
   * fixture covers. The body is rover output, pasted verbatim.
   */
  private def inlineSupergraph(body: String): String =
    s"""schema
       |  @link(url: "https://specs.apollo.dev/link/v1.0")
       |  @link(url: "https://specs.apollo.dev/join/v0.5", for: EXECUTION)
       |{
       |  query: Query
       |}
       |
       |enum join__Graph {
       |  A @join__graph(name: "a", url: "http://a/graphql")
       |  B @join__graph(name: "b", url: "http://b/graphql")
       |}
       |
       |type Query @join__type(graph: A) @join__type(graph: B) {
       |  widget: Widget @join__field(graph: A)
       |  probe: Int @join__field(graph: B)
       |}
       |
       |$body
       |""".stripMargin

  private def composeProjections(sdl: String): UIO[ComposedGraph] =
    for {
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
        supergraph <- characterGraphFromSupergraph
        originals  <- characterGraphFromOriginals
      } yield assertTrue(render(supergraph) == render(originals))
    },
    test("decomposing then composing yields the same routing as composing the originals") {
      // Schema equality above cannot see ownership: this is the assertion that fails if the field
      // inclusion rule is inverted.
      def sorted[K](routes: Map[K, List[ComposedGraph.FieldRoute]]) = routes.view.mapValues(_.sortBy(_.source)).toMap

      for {
        supergraph <- characterGraphFromSupergraph
        originals  <- characterGraphFromOriginals
      } yield assertTrue(
        sorted(supergraph.routes.view.mapValues(_.providers).toMap) == sorted(
          originals.routes.view.mapValues(_.providers).toMap
        ),
        sorted(supergraph.fieldRoutes) == sorted(originals.fieldRoutes)
      )
    },
    test("routes each root field to the graph that declared it") {
      characterGraphFromSupergraph.map { graph =>
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
      characterGraphFromSupergraph.map { graph =>
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
      characterGraphFromSupergraph.map { graph =>
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
    },
    // A progressive `@override` is the one override the overridden graph must keep resolving, and
    // the composer enforces that: a progressive override whose `from` graph does not own the field
    // is a composition error, not a warning.
    suite("progressive overrides")(
      test("leaves the overridden graph owning the field so the rollout can route to it") {
        // Rover writes a progressive override into *both* graphs: the overriding graph gets
        // `override:` with `overrideLabel:`, and the overridden graph gets a bare `overrideLabel:`
        // entry, with no `external:` and no `usedOverridden:`.
        composeProjections(
          inlineSupergraph(
            """type Widget @join__type(graph: A, key: "id") @join__type(graph: B, key: "id") {
              |  id: ID!
              |  price: Float! @join__field(graph: A, override: "b", overrideLabel: "percent(25)")
              |    @join__field(graph: B, overrideLabel: "percent(25)")
              |  colour: String! @join__field(graph: A, override: "b", overrideLabel: "myFlag")
              |    @join__field(graph: B, overrideLabel: "myFlag")
              |  weight: Float! @join__field(graph: A, override: "b")
              |}""".stripMargin
          )
        ).map { graph =>
          def routes(field: String) =
            graph.fieldRoutes.getOrElse("Widget" -> field, Nil).map(route => route.source -> route.condition)

          val percent = ComposedGraph.OverrideLabel("percent(25)")
          val flag    = ComposedGraph.OverrideLabel("myFlag")

          assertTrue(
            routes("price") == List(
              "a" -> Some(ComposedGraph.OverrideCondition(percent, Some(BigDecimal(25)), active = true)),
              "b" -> Some(ComposedGraph.OverrideCondition(percent, Some(BigDecimal(25)), active = false))
            ),
            // A custom label carries no percentage: the gateway resolves it per request instead.
            routes("colour") == List(
              "a" -> Some(ComposedGraph.OverrideCondition(flag, None, active = true)),
              "b" -> Some(ComposedGraph.OverrideCondition(flag, None, active = false))
            ),
            // A plain override leaves the overridden graph with no entry at all, so `b` never
            // declares the field and the route is unconditional.
            routes("weight") == List("a" -> None),
            graph.progressiveOverrides(Set("price", "colour")) ==
              Map(percent -> Some(BigDecimal(25)), flag -> None)
          )
        }
      }
    ),
    suite("contexts")(
      test("decomposing then composing yields the same schema and routing as composing the originals") {
        def sorted[K](routes: Map[K, List[ComposedGraph.FieldRoute]]) = routes.view.mapValues(_.sortBy(_.source)).toMap

        for {
          supergraph <- contextGraphFromSupergraph
          originals  <- contextGraphFromOriginals
        } yield assertTrue(
          render(supergraph) == render(originals),
          sorted(supergraph.fieldRoutes) == sorted(originals.fieldRoutes)
        )
      },
      test("reaches the same context declarations and arguments as composing the originals") {
        // The assertion the schema comparison cannot make: a context argument is hidden from the
        // composed field, so a projection that lost one composes to a byte-identical schema that
        // cannot resolve the field at all.
        def declarations(graph: ComposedGraph) =
          graph.contextDeclarations("Character").map(value => (value.source, value.typeName, value.name.value)).sorted

        for {
          supergraph <- contextGraphFromSupergraph
          originals  <- contextGraphFromOriginals
        } yield assertTrue(
          declarations(supergraph) == declarations(originals),
          supergraph.fromContext("characters", "Ship", "fare") ==
            originals.fromContext("characters", "Ship", "fare"),
          supergraph.fromContext("episodes", "Ship", "manifest") ==
            originals.fromContext("episodes", "Ship", "manifest")
        )
      },
      test("composes the names the subgraphs wrote rather than the namespaced supergraph ones") {
        contextGraphFromSupergraph.map { graph =>
          assertTrue(
            graph.contextDeclarations("Character").map(value => value.source -> value.name.value).sorted ==
              List("characters" -> "viewer", "episodes" -> "crew").sorted,
            graph.fromContext("characters", "Ship", "fare").map(value => value.argument -> value.context.value) ==
              List("currency" -> "viewer", "locale" -> "viewer"),
            graph.fromContext("episodes", "Ship", "manifest").map(value => value.argument -> value.context.value) ==
              List("rank" -> "crew")
          )
        }
      }
    )
  )
}
