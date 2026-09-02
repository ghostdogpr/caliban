package caliban.gateway

import caliban.gateway.internal.composition.SupergraphDecomposition
import caliban.gateway.internal.composition.SupergraphDecomposition.Graph
import caliban.InputValue
import caliban.Value.StringValue
import caliban.parsing.Parser
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Directive, Document }
import caliban.rendering.DocumentRenderer
import sttp.model.Uri
import zio._
import zio.test._

import scala.io.Source

object SupergraphDecompositionSpec extends ZIOSpecDefault {

  private val JoinV05 = "https://specs.apollo.dev/join/v0.5"

  /** Wraps a body in the minimum schema definition that makes a document look like a supergraph. */
  private def supergraph(body: String, join: String = JoinV05, as: String = ""): String = {
    val alias = if (as.isEmpty) "" else s""", as: "$as""""
    s"""schema
       |  @link(url: "https://specs.apollo.dev/link/v1.0")
       |  @link(url: "$join"$alias, for: EXECUTION)
       |{
       |  query: Query
       |}
       |
       |type Query { hello: String }
       |
       |$body
       |""".stripMargin
  }

  /** As [[supergraph]], but also linking the context feature so `@context` resolves to it. */
  private def contextSupergraph(body: String): String =
    s"""schema
       |  @link(url: "https://specs.apollo.dev/link/v1.0")
       |  @link(url: "$JoinV05", for: EXECUTION)
       |  @link(url: "https://specs.apollo.dev/context/v0.1", for: SECURITY)
       |{
       |  query: Query
       |}
       |
       |directive @context(name: String!) repeatable on INTERFACE | OBJECT | UNION
       |directive @context__fromContext(field: context__ContextFieldValue) on ARGUMENT_DEFINITION
       |
       |scalar context__ContextFieldValue
       |
       |type Query { hello: String }
       |
       |$body
       |""".stripMargin

  /** As [[supergraph]], but naming a subscription root so the shared-Subscription guard applies. */
  private def subscriptionSupergraph(body: String): String =
    s"""schema
       |  @link(url: "https://specs.apollo.dev/link/v1.0")
       |  @link(url: "$JoinV05", for: EXECUTION)
       |{
       |  query: Query
       |  subscription: Subscription
       |}
       |
       |enum join__Graph {
       |  A @join__graph(name: "a", url: "http://a/graphql")
       |  B @join__graph(name: "b", url: "http://b/graphql")
       |}
       |
       |type Query @join__type(graph: A) @join__type(graph: B) { hello: String }
       |
       |$body
       |""".stripMargin

  private def graphs(sdl: String): UIO[Either[List[String], List[Graph]]] =
    ZIO.fromEither(Parser.parseQuery(sdl)).orDie.map(SupergraphDecomposition.graphs)

  private def resource(name: String): UIO[String] =
    ZIO
      .scoped(ZIO.fromAutoCloseable(ZIO.attempt(Source.fromResource(s"supergraph/$name"))).map(_.mkString))
      .orDie

  private val fixtureSdl: UIO[String] = resource("supergraph.graphql")

  /** The same two subgraphs composed by Hive's composer rather than rover's. See the fixture README. */
  private val hiveFixtureSdl: UIO[String] = resource("supergraph-hive.graphql")

  private val fixture: UIO[Either[List[String], List[Graph]]] = fixtureSdl.flatMap(graphs)

  private def decompose(sdl: String): UIO[Either[List[String], Map[String, Document]]] =
    ZIO
      .fromEither(Parser.parseQuery(sdl))
      .orDie
      .map(SupergraphDecomposition.decompose(_).map(_.map(entry => entry.graph.name -> entry.document).toMap))

  /** The fixture projected into one document per subgraph; dies with the diagnostics on failure. */
  private def projectionOf(sdl: UIO[String]): UIO[Map[String, Document]] =
    sdl.flatMap(decompose).flatMap {
      case Right(value) => ZIO.succeed(value)
      case Left(errors) => ZIO.die(new AssertionError(s"decomposition failed: ${errors.mkString("; ")}"))
    }

  private val projected: UIO[Map[String, Document]] = projectionOf(fixtureSdl)

  /** The rover-composed `@context`/`@fromContext` fixture, projected. See the fixture README. */
  private val contextProjected: UIO[Map[String, Document]] =
    projectionOf(resource("context-supergraph.graphql"))

  /**
   * Definitions rendered one at a time and sorted, so two projections compare on content rather than
   * on the order their composer happened to emit: rover sorts its output, Hive's composer does not.
   */
  private def canonical(document: Document): List[String] =
    document.definitions.map(definition => DocumentRenderer.render(Document(List(definition), document.sourceMapper)))

  /** What `left` projects that `right` does not, so a failure reads as the difference itself. */
  private def missing(left: Document, right: Document): List[String] =
    canonical(left).sorted.diff(canonical(right).sorted)

  private def types(document: Document): Set[String] = document.typeDefinitions.map(_.name).toSet

  private def fields(document: Document, typeName: String): List[String] =
    document.objectTypeDefinitions.filter(_.name == typeName).flatMap(_.fields.map(_.name))

  private def fieldDirectives(document: Document, typeName: String, fieldName: String): List[String] =
    document.objectTypeDefinitions
      .filter(_.name == typeName)
      .flatMap(_.fields.filter(_.name == fieldName))
      .flatMap(_.directives.map(_.name))

  private def directiveOn(document: Document, typeName: String, name: String): Option[Directive] =
    document.typeDefinitions.find(_.name == typeName).flatMap(_.directives.find(_.name == name))

  private def fieldDirective(
    document: Document,
    typeName: String,
    fieldName: String,
    name: String
  ): Option[Directive] =
    document.objectTypeDefinitions
      .find(_.name == typeName)
      .flatMap(_.fields.find(_.name == fieldName))
      .flatMap(_.directives.find(_.name == name))

  private def contextNames(document: Document, typeName: String): List[String] =
    document.typeDefinitions
      .filter(_.name == typeName)
      .flatMap(_.directives.filter(_.name == "context"))
      .flatMap(_.arguments.get("name").collect { case StringValue(value) => value })

  private def arguments(document: Document, typeName: String, fieldName: String): List[InputValueDefinition] =
    document.objectTypeDefinitions
      .filter(_.name == typeName)
      .flatMap(_.fields.filter(_.name == fieldName))
      .flatMap(_.args)

  /** Each argument's reconstructed `@fromContext` selection, by argument name. */
  private def fromContext(document: Document, typeName: String, fieldName: String): List[(String, String)] =
    arguments(document, typeName, fieldName).flatMap(argument =>
      argument.directives
        .filter(_.name == "fromContext")
        .flatMap(_.arguments.get("field").collect { case StringValue(value) => value })
        .map(argument.name -> _)
    )

  private def typeDirectives(document: Document, typeName: String, name: String): List[String] =
    document.typeDefinitions
      .filter(_.name == typeName)
      .flatMap(_.directives.filter(_.name == name))
      .flatMap(_.arguments.get("fields").collect { case StringValue(value) => value })

  /** Projects to name -> url so the assertions survive `Graph` gaining fields in task 6a. */
  private def entries(result: Either[List[String], List[Graph]]): Either[List[String], List[(String, String)]] =
    result.map(_.map(graph => graph.name -> graph.url.toString))

  def spec = suite("SupergraphDecompositionSpec")(
    suite("graph registry")(
      test("resolves every subgraph declared by the join graph enum, in declaration order") {
        fixture.map(result =>
          assertTrue(
            entries(result) == Right(
              List(
                "characters" -> "http://127.0.0.1:9008/graphql/federated",
                "episodes"   -> "http://127.0.0.1:9009/graphql/federated"
              )
            )
          )
        )
      },
      test("resolves a minimal supergraph") {
        graphs(supergraph("""enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }"""))
          .map(result => assertTrue(entries(result) == Right(List("a" -> "http://a/graphql"))))
      },
      test("honours an aliased join namespace rather than a hardcoded prefix") {
        graphs(
          supergraph(
            """enum j__Graph { A @j__graph(name: "a", url: "http://a/graphql") }""",
            as = "j"
          )
        ).map(result => assertTrue(entries(result) == Right(List("a" -> "http://a/graphql"))))
      },
      test("preserves the declared url path, port and query") {
        graphs(
          supergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "https://host:8443/base/graphql?tenant=x") }"""
          )
        ).map(result => assertTrue(entries(result) == Right(List("a" -> "https://host:8443/base/graphql?tenant=x"))))
      }
    ),
    suite("hard stops")(
      test("rejects a document that does not @link the join feature") {
        graphs(
          """schema @link(url: "https://specs.apollo.dev/link/v1.0") { query: Query }
            |type Query { hello: String }
            |""".stripMargin
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] The document does not @link the join feature and is not a supergraph.")
            )
          )
        )
      },
      test("rejects a document with no schema definition at all") {
        graphs("type Query { hello: String }").map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] The document does not @link the join feature and is not a supergraph.")
            )
          )
        )
      },
      test("rejects a join-linked document whose graph enum is missing") {
        graphs(supergraph("")).map(result =>
          assertTrue(result == Left(List("[supergraph] The join graph enum 'join__Graph' is missing.")))
        )
      },
      test("names the aliased graph enum it could not find") {
        graphs(supergraph("", as = "j")).map(result =>
          assertTrue(result == Left(List("[supergraph] The join graph enum 'j__Graph' is missing.")))
        )
      },
      test("reports a hard stop without also reporting per-value diagnostics") {
        graphs(supergraph("enum other__Graph { A }")).map(result => assertTrue(result.left.exists(_.size == 1)))
      }
    ),
    suite("per-value diagnostics")(
      test("rejects an enum value carrying no graph directive") {
        graphs(supergraph("enum join__Graph { A }")).map(result =>
          assertTrue(result == Left(List("[supergraph] Join graph 'A' has no graph directive.")))
        )
      },
      test("rejects a missing name argument") {
        graphs(supergraph("""enum join__Graph { A @join__graph(url: "http://a/graphql") }""")).map(result =>
          assertTrue(
            result == Left(List("[supergraph] Join graph 'A' must declare a non-empty 'name' argument."))
          )
        )
      },
      test("rejects a blank name argument") {
        graphs(supergraph("""enum join__Graph { A @join__graph(name: "   ", url: "http://a/graphql") }""")).map(
          result =>
            assertTrue(
              result == Left(List("[supergraph] Join graph 'A' must declare a non-empty 'name' argument."))
            )
        )
      },
      test("rejects a non-string name argument") {
        graphs(supergraph("""enum join__Graph { A @join__graph(name: 7, url: "http://a/graphql") }""")).map(result =>
          assertTrue(
            result == Left(List("[supergraph] Join graph 'A' must declare a non-empty 'name' argument."))
          )
        )
      },
      test("rejects a missing url argument") {
        graphs(supergraph("""enum join__Graph { A @join__graph(name: "a") }""")).map(result =>
          assertTrue(result == Left(List("[supergraph] Join graph 'A' must declare a 'url' argument.")))
        )
      },
      // The point of the pair-match in `graphEntry`: both arguments are validated independently,
      // so a value missing both reports two diagnostics. A for-comprehension would report one.
      test("accumulates both diagnostics when name and url are absent") {
        graphs(supergraph("enum join__Graph { A @join__graph }")).map(result =>
          assertTrue(
            result == Left(
              List(
                "[supergraph] Join graph 'A' must declare a 'url' argument.",
                "[supergraph] Join graph 'A' must declare a non-empty 'name' argument."
              )
            )
          )
        )
      },
      test("accumulates diagnostics across every offending enum value") {
        graphs(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a")
              |  B @join__graph(url: "http://b/graphql")
              |  C @join__graph(name: "c", url: "http://c/graphql")
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List(
                "[supergraph] Join graph 'A' must declare a 'url' argument.",
                "[supergraph] Join graph 'B' must declare a non-empty 'name' argument."
              )
            )
          )
        )
      }
    ),
    suite("url validation")(
      // sttp's Uri.parse is far more lenient than it looks: "foo", "not a uri", "://nohost" and
      // "http://" all parse successfully into relative or hostless URIs. Parsing alone is therefore
      // not validation -- an unusable endpoint would surface much later as a confusing request-time
      // failure. A subgraph endpoint must be an absolute http(s) URI with a non-empty host.
      test("rejects any url that is not an absolute http or https endpoint") {
        val invalid  = List(
          "",
          "%%%",
          "foo",
          "not a uri",
          "   ",
          "://nohost",
          "http://",
          "//host/graphql",
          "mailto:someone@example.com"
        )
        val expected = Left(List("[supergraph] Join graph 'A' must declare an absolute http or https 'url'."))
        ZIO
          .foreach(invalid)(url =>
            graphs(supergraph(s"""enum join__Graph { A @join__graph(name: "a", url: "$url") }""")).map(url -> _)
          )
          .map(results => assertTrue(results.filterNot(_._2 == expected).map(_._1) == Nil))
      },
      test("accepts http and https endpoints, with or without a port and path") {
        val valid = List(
          "http://a/graphql",
          "https://a/graphql",
          "http://127.0.0.1:9008/graphql/federated",
          "https://host:8443/base/graphql?tenant=x"
        )
        ZIO
          .foreach(valid)(url =>
            graphs(supergraph(s"""enum join__Graph { A @join__graph(name: "a", url: "$url") }"""))
              .map(result => url -> entries(result))
          )
          .map(results => assertTrue(results.filterNot { case (url, r) => r == Right(List("a" -> url)) } == Nil))
      }
    ),
    suite("cross-value checks")(
      test("rejects an empty graph enum") {
        graphs(supergraph("enum join__Graph")).map(result =>
          assertTrue(
            result == Left(List("[supergraph] The join graph enum 'join__Graph' declares no subgraphs."))
          )
        )
      },
      test("rejects a subgraph name declared more than once") {
        graphs(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "a", url: "http://b/graphql")
              |}""".stripMargin
          )
        ).map(result => assertTrue(result == Left(List("[supergraph] Subgraph name 'a' is declared more than once."))))
      },
      test("allows the same url under two distinct names") {
        graphs(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://shared/graphql")
              |  B @join__graph(name: "b", url: "http://shared/graphql")
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            entries(result) == Right(List("a" -> "http://shared/graphql", "b" -> "http://shared/graphql"))
          )
        )
      },
      test("sorts and de-duplicates the reported diagnostics") {
        graphs(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a")
              |  B @join__graph(name: "a")
              |}""".stripMargin
          )
        ).map { result =>
          val diagnostics = result.left.getOrElse(Nil)
          assertTrue(diagnostics == diagnostics.distinct.sorted, diagnostics.nonEmpty)
        }
      }
    ),
    suite("projection: type membership")(
      test("keeps a type only in the graphs its join type entries name") {
        projected.map { graphs =>
          val characters        = types(graphs("characters"))
          val episodes          = types(graphs("episodes"))
          val ownedByCharacters = Set("Captain", "Engineer", "Mechanic", "Pilot", "Origin", "Role")
          assertTrue(
            ownedByCharacters.subsetOf(characters),
            (ownedByCharacters intersect episodes) == Set.empty[String],
            characters.contains("Character"),
            episodes.contains("Character"),
            characters.contains("Episode"),
            episodes.contains("Episode")
          )
        }
      },
      test("strips every join and link definition, application and type") {
        projected.map { graphs =>
          val rendered = graphs.values.map(DocumentRenderer.render).toList
          assertTrue(
            rendered.forall(!_.contains("join__")),
            rendered.forall(!_.contains("link__")),
            // The supergraph's own @link applications are stripped; the only one that survives is
            // the federation link synthesised for the subgraph.
            rendered.forall(!_.contains("specs.apollo.dev/link/")),
            rendered.forall(!_.contains("specs.apollo.dev/join/")),
            rendered.forall(!_.contains("specs.apollo.dev/inaccessible/")),
            rendered.forall(_.split("@link").length == 2),
            rendered.forall(!_.contains("@inaccessible\n\ndirective")),
            graphs.values.forall(_.directiveDefinitions.isEmpty)
          )
        }
      }
    ),
    suite("projection: field ownership")(
      // The default-ownership case: no @join__field at all means every member graph resolves the
      // field. `Episode.season` and `Episode.episode` are the fixture's instance of it.
      test("gives a field with no join field directive to every member graph") {
        projected.map { graphs =>
          assertTrue(
            fields(graphs("characters"), "Episode") == List("season", "episode", "characters", "leader"),
            fields(graphs("episodes"), "Episode") == List("season", "episode", "name")
          )
        }
      },
      test("gives a scoped field only to the graphs that name it") {
        projected.map { graphs =>
          assertTrue(
            fields(graphs("characters"), "Character") ==
              List("name", "nicknames", "origin", "role", "starredIn", "biography"),
            fields(graphs("episodes"), "Character") == List("name", "isCaptain")
          )
        }
      },
      test("translates a repeated join field entry per graph") {
        projected.map { graphs =>
          assertTrue(
            fieldDirectives(graphs("episodes"), "Character", "name") == List("external"),
            fieldDirectives(graphs("characters"), "Character", "name") == Nil
          )
        }
      },
      test("retains non-join directives on a projected field") {
        projected.map { graphs =>
          assertTrue(fieldDirectives(graphs("characters"), "Character", "biography") == List("inaccessible"))
        }
      },
      test("splits root fields across the graphs that own them") {
        projected.map { graphs =>
          assertTrue(
            fields(graphs("characters"), "Query") == List("characters", "character"),
            fields(graphs("episodes"), "Query") == List("episode", "episodes")
          )
        }
      },
      test("names the query root in both subgraphs") {
        projected.map { graphs =>
          assertTrue(
            graphs.values.forall(_.schemaDefinition.flatMap(_.query).contains("Query")),
            graphs.values.forall(_.schemaDefinition.flatMap(_.mutation).isEmpty),
            graphs.values.forall(_.schemaDefinition.flatMap(_.subscription).isEmpty)
          )
        }
      }
    ),
    suite("projection: metadata translation")(
      test("translates every join type key into a key directive") {
        projected.map { graphs =>
          assertTrue(
            typeDirectives(graphs("characters"), "Character", "key") == List("name"),
            typeDirectives(graphs("episodes"), "Character", "key") == List("name"),
            typeDirectives(graphs("characters"), "Episode", "key") == List("season episode"),
            typeDirectives(graphs("episodes"), "Episode", "key") == List("season episode")
          )
        }
      },
      test("filters union members and enum values by graph") {
        projected.map { graphs =>
          val role   = graphs("characters").unionTypeDefinitions.find(_.name == "Role")
          val origin = graphs("characters").enumTypeDefinitions.find(_.name == "Origin")
          assertTrue(
            role.map(_.memberTypes) == Some(List("Captain", "Pilot", "Engineer", "Mechanic")),
            origin.map(_.enumValuesDefinition.map(_.enumValue)) == Some(List("EARTH", "MARS", "BELT")),
            origin.toList.flatMap(_.enumValuesDefinition).forall(_.directives.isEmpty)
          )
        }
      },
      test("overrides a field type when the join field declares one") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A) @join__type(graph: B) {
              |  size: Int! @join__field(graph: A) @join__field(graph: B, type: "String")
              |}""".stripMargin
          )
        ).map { result =>
          def sizeType(graphs: Map[String, Document], graph: String) =
            graphs(graph).objectTypeDefinitions
              .find(_.name == "Widget")
              .flatMap(_.fields.find(_.name == "size"))
              .map(_.ofType.toString)

          assertTrue(
            result.map(sizeType(_, "a")) == Right(Some("Int!")),
            result.map(sizeType(_, "b")) == Right(Some("String"))
          )
        }
      },
      test("keeps the full per-graph list when the filter directive is absent everywhere") {
        // Older join versions omit @join__unionMember and @join__enumValue: absent must mean
        // "shared by every graph", not "owned by none".
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Captain @join__type(graph: A) @join__type(graph: B) { rank: String! }
              |type Pilot @join__type(graph: A) @join__type(graph: B) { hours: Int! }
              |union Role @join__type(graph: A) @join__type(graph: B) = Captain | Pilot
              |enum Origin @join__type(graph: A) @join__type(graph: B) { EARTH MARS }""".stripMargin
          )
        ).map { result =>
          def members(graphs: Map[String, Document], graph: String) =
            graphs(graph).unionTypeDefinitions.find(_.name == "Role").map(_.memberTypes)
          def values(graphs: Map[String, Document], graph: String)  =
            graphs(graph).enumTypeDefinitions.find(_.name == "Origin").map(_.enumValuesDefinition.map(_.enumValue))

          assertTrue(
            result.map(members(_, "a")) == Right(Some(List("Captain", "Pilot"))),
            result.map(members(_, "b")) == Right(Some(List("Captain", "Pilot"))),
            result.map(values(_, "a")) == Right(Some(List("EARTH", "MARS"))),
            result.map(values(_, "b")) == Right(Some(List("EARTH", "MARS")))
          )
        }
      }
    ),
    suite("projection: federation link")(
      test("links the federation feature in every projection") {
        // Without this every directive-name set in SchemaComposer.federationDirectiveNames comes
        // back empty and isFederation2 is false, so the projections compose as ordinary graphs
        // with no entity lookups instead of failing loudly.
        projected.map { graphs =>
          def link(document: Document) =
            document.schemaDefinition.toList
              .flatMap(_.directives)
              .filter(_.name == "link")
              .flatMap(_.arguments.get("url").collect { case StringValue(value) => value })

          assertTrue(
            graphs.values.forall(link(_) == List("https://specs.apollo.dev/federation/v2.9"))
          )
        }
      },
      test("imports every federation directive the projection can emit, exactly once") {
        projected.map { graphs =>
          val imported = graphs("characters").schemaDefinition.toList
            .flatMap(_.directives)
            .find(_.name == "link")
            .flatMap(_.arguments.get("import"))
            .collect { case InputValue.ListValue(values) =>
              values.collect { case StringValue(value) => value }
            }
            .getOrElse(Nil)

          assertTrue(
            imported.distinct == imported,
            // The version must clear the composer's minimums: @authenticated/@requiresScopes need
            // federation >= 2.5 and @policy >= 2.6.
            // @context and @fromContext need federation >= 2.8.
            Set(
              "@key",
              "@shareable",
              "@external",
              "@override",
              "@inaccessible",
              "@authenticated",
              "@policy",
              "@context",
              "@fromContext"
            ).subsetOf(imported.toSet)
          )
        }
      },
      test("renders and reparses when a graph contributes no operation root") {
        // The schema definition is now emitted unconditionally, so a graph holding only the link
        // and no roots must still render as SDL a parser accepts.
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A) @join__type(graph: B) {
              |  size: Int! @join__field(graph: A)
              |}""".stripMargin
          )
        ).map { result =>
          val rendered = result.map(graphs => DocumentRenderer.render(graphs("b")))
          assertTrue(
            rendered.map(_.contains("Widget")) == Right(false),
            rendered.map(value => Parser.parseQuery(value).isRight) == Right(true)
          )
        }
      }
    ),
    suite("projection: shareable synthesis")(
      test("marks a field resolved by two graphs shareable in both projections") {
        // Without this the composer rejects the pair: "resolved by multiple subgraphs without
        // compatible @shareable declarations". `Episode.season` carries no @join__field, so it
        // lands in every graph that declares the type.
        projected.map { graphs =>
          assertTrue(
            fieldDirectives(graphs("characters"), "Episode", "season").contains("shareable"),
            fieldDirectives(graphs("episodes"), "Episode", "season").contains("shareable")
          )
        }
      },
      test("never marks an external field shareable") {
        // `Character.name` is owned by CHARACTERS and external in EPISODES, so EPISODES does not
        // resolve it and neither projection has two providers.
        projected.map { graphs =>
          assertTrue(
            fieldDirectives(graphs("episodes"), "Character", "name") == List("external"),
            !fieldDirectives(graphs("characters"), "Character", "name").contains("shareable")
          )
        }
      },
      test("discounts a graph another graph has overridden away") {
        // The only difference between the two documents is `override:`. With it there is one
        // effective provider, so neither projection may claim shareability.
        def widget(overrides: String) =
          supergraph(
            s"""enum join__Graph {
               |  A @join__graph(name: "a", url: "http://a/graphql")
               |  B @join__graph(name: "b", url: "http://b/graphql")
               |}
               |type Widget @join__type(graph: A) @join__type(graph: B) {
               |  size: Int! @join__field(graph: A$overrides) @join__field(graph: B)
               |}""".stripMargin
          )

        def shareable(result: Either[List[String], Map[String, Document]], graph: String) =
          result.map(_(graph)).map(fieldDirectives(_, "Widget", "size").contains("shareable"))

        for {
          overridden <- decompose(widget(""", override: "b""""))
          plain      <- decompose(widget(""))
        } yield assertTrue(
          shareable(overridden, "a") == Right(false),
          shareable(overridden, "b") == Right(false),
          shareable(plain, "a") == Right(true),
          shareable(plain, "b") == Right(true)
        )
      },
      test("rejects a subscription root field resolved by more than one graph") {
        // The composer fails these with "Subscription fields require one effective owner and
        // cannot be @shareable"; naming the field here is more useful than that.
        decompose(
          subscriptionSupergraph(
            "type Subscription @join__type(graph: A) @join__type(graph: B) { ticks: Int! }"
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List(
                "[supergraph] Subscription field 'Subscription.ticks' is resolved by more than one graph, " +
                  "which the gateway cannot route."
              )
            )
          )
        )
      },
      test("accepts a subscription root field owned by a single graph") {
        decompose(
          subscriptionSupergraph(
            """type Subscription @join__type(graph: A) @join__type(graph: B) {
              |  ticks: Int! @join__field(graph: A)
              |}""".stripMargin
          )
        ).map { result =>
          def subscriptionRoot(document: Document) = document.schemaDefinition.flatMap(_.subscription)

          assertTrue(
            result.map(graphs => fields(graphs("a"), "Subscription")) == Right(List("ticks")),
            result.map(graphs => subscriptionRoot(graphs("a"))) == Right(Some("Subscription")),
            result.map(graphs => types(graphs("b")).contains("Subscription")) == Right(false),
            result.map(graphs => subscriptionRoot(graphs("b"))) == Right(None)
          )
        }
      },
      test("drops every kind of type that would project without members") {
        // GraphQL forbids an empty object, interface, union, enum or input object. A graph that is
        // a member of the type but owns none of its members must not emit a bare `type Widget`.
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A) @join__type(graph: B) {
              |  size: Int! @join__field(graph: A)
              |}
              |interface Named @join__type(graph: A) @join__type(graph: B) {
              |  name: String! @join__field(graph: A)
              |}
              |input Filter @join__type(graph: A) @join__type(graph: B) {
              |  size: Int @join__field(graph: A)
              |}
              |type Captain @join__type(graph: A) @join__type(graph: B) { rank: String! }
              |union Role @join__type(graph: A) @join__type(graph: B)
              |  @join__unionMember(graph: A, member: "Captain") = Captain
              |enum Origin @join__type(graph: A) @join__type(graph: B) {
              |  EARTH @join__enumValue(graph: A)
              |}""".stripMargin
          )
        ).map { result =>
          val absent = Set("Widget", "Named", "Filter", "Role", "Origin")
          assertTrue(
            result.map(graphs => absent.forall(types(graphs("a")).contains)) == Right(true),
            result.map(graphs => types(graphs("b")).intersect(absent)) == Right(Set.empty[String])
          )
        }
      },
      test("leaves an unannotated subscription root to the composer") {
        // A type with no @join__type is a `join/v0.1` shared value type, where ownership is not
        // expressible. Rejecting those wholesale would fail every Federation 1 supergraph, so the
        // guard only applies where the supergraph annotated the root.
        decompose(subscriptionSupergraph("type Subscription { ticks: Int! }"))
          .map(result => assertTrue(result.isRight))
      }
    ),
    suite("projection: validation")(
      test("rejects a join type entry naming a graph the enum omits") {
        decompose(
          supergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: MISSING) { size: Int! }""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] Type 'Widget' names graph 'MISSING', which the graph enum omits.")
            )
          )
        )
      },
      test("rejects a join field entry naming a graph the type does not declare") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A) { size: Int! @join__field(graph: B) }""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] Field 'Widget.size' names graph 'B', which does not declare the type.")
            )
          )
        )
      },
      test("rejects two join field entries for the same graph") {
        decompose(
          supergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: A) {
              |  size: Int! @join__field(graph: A) @join__field(graph: A, external: true)
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] Field 'Widget.size' declares more than one entry for graph 'A'.")
            )
          )
        )
      },
      test("rejects an unparseable join field type override") {
        decompose(
          supergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: A) { size: Int! @join__field(graph: A, type: "[[[") }""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(List("[supergraph] Field 'Widget.size' declares the unparseable type '[[['."))
          )
        )
      },
      test("rejects a document declaring type extensions") {
        decompose(
          supergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: A) { size: Int! }
              |extend type Widget { extra: String }""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] A supergraph is fully composed and must not declare type extensions.")
            )
          )
        )
      },
      test("accumulates diagnostics across types and fields") {
        decompose(
          supergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: MISSING) { size: Int! }
              |type Gadget @join__type(graph: A) { size: Int! @join__field(graph: OTHER) }""".stripMargin
          )
        ).map(result => assertTrue(result.left.exists(_.size == 2)))
      },
      test("propagates the registry hard stops unchanged") {
        decompose("type Query { hello: String }").map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] The document does not @link the join feature and is not a supergraph.")
            )
          )
        )
      }
    ),
    // Paths the fixture does not exercise, each a distinct branch of the projection.
    suite("projection: paths absent from the fixture")(
      test("translates requires, provides, override, external and usedOverridden") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A, key: "id") @join__type(graph: B, key: "id") {
              |  id: ID!
              |  price: Float! @join__field(graph: A, requires: "weight", provides: "vendor")
              |  weight: Float! @join__field(graph: A, external: true) @join__field(graph: B)
              |  label: String! @join__field(graph: A, override: "b") @join__field(graph: B, usedOverridden: true)
              |}""".stripMargin
          )
        ).map {
          case Left(errors)  => assertTrue(errors == Nil)
          case Right(graphs) =>
            val a = graphs("a")
            val b = graphs("b")
            assertTrue(
              fieldDirective(a, "Widget", "price", "requires").flatMap(_.arguments.get("fields")) ==
                Some(StringValue("weight")),
              fieldDirective(a, "Widget", "price", "provides").flatMap(_.arguments.get("fields")) ==
                Some(StringValue("vendor")),
              fieldDirective(a, "Widget", "weight", "external").isDefined,
              fieldDirective(b, "Widget", "weight", "external").isEmpty,
              fieldDirective(a, "Widget", "label", "override").flatMap(_.arguments.get("from")) ==
                Some(StringValue("b")),
              fieldDirective(b, "Widget", "label", "external").isDefined
            )
        }
      },
      test("translates a progressive override label onto the overriding graph only") {
        // Rover writes the label into both graphs — the overridden graph gets a bare
        // `overrideLabel:` entry — but only the graph carrying `override:` can express it, and
        // the overridden graph must stay a plain owner for the rollout to have anywhere to route.
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A, key: "id") @join__type(graph: B, key: "id") {
              |  id: ID!
              |  price: Float! @join__field(graph: A, override: "b", overrideLabel: "percent(25)")
              |    @join__field(graph: B, overrideLabel: "percent(25)")
              |}""".stripMargin
          )
        ).map {
          case Left(errors)  => assertTrue(errors == Nil)
          case Right(graphs) =>
            assertTrue(
              fieldDirective(graphs("a"), "Widget", "price", "override").map(_.arguments) == Some(
                Map[String, InputValue](
                  "from"  -> StringValue("b"),
                  "label" -> StringValue("percent(25)")
                )
              ),
              fieldDirectives(graphs("b"), "Widget", "price") == Nil
            )
        }
      },
      test("emits resolvable false and interfaceObject") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Media @join__type(graph: A, key: "id", isInterfaceObject: true)
              |  @join__type(graph: B, key: "id", resolvable: false) {
              |  id: ID!
              |  title: String!
              |}""".stripMargin
          )
        ).map { result =>
          def key(graphs: Map[String, Document], graph: String) =
            directiveOn(graphs(graph), "Media", "key").map(_.arguments)

          assertTrue(
            result.map(key(_, "a")) == Right(Some(Map[String, caliban.InputValue]("fields" -> StringValue("id")))),
            result.map(key(_, "b")) == Right(
              Some(
                Map[String, caliban.InputValue](
                  "fields"     -> StringValue("id"),
                  "resolvable" -> caliban.Value.BooleanValue(false)
                )
              )
            ),
            result.map(graphs => directiveOn(graphs("a"), "Media", "interfaceObject").isDefined) == Right(true),
            result.map(graphs => directiveOn(graphs("b"), "Media", "interfaceObject").isDefined) == Right(false)
          )
        }
      },
      test("re-emits a composed directive only into the graphs it names, and keeps its definition") {
        decompose(
          supergraph(
            """directive @cacheTag(format: String!) on OBJECT | FIELD_DEFINITION | SCHEMA
              |enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Widget @join__type(graph: A) @join__type(graph: B)
              |  @join__directive(graphs: [A], name: "cacheTag", args: {format: "widget"}) {
              |  size: Int! @join__directive(graphs: [B], name: "cacheTag", args: {format: "size"})
              |}""".stripMargin
          )
        ).map {
          case Left(errors)  => assertTrue(errors == Nil)
          case Right(graphs) =>
            assertTrue(
              directiveOn(graphs("a"), "Widget", "cacheTag").flatMap(_.arguments.get("format")) ==
                Some(StringValue("widget")),
              directiveOn(graphs("b"), "Widget", "cacheTag").isEmpty,
              fieldDirective(graphs("b"), "Widget", "size", "cacheTag").flatMap(_.arguments.get("format")) ==
                Some(StringValue("size")),
              fieldDirective(graphs("a"), "Widget", "size", "cacheTag").isEmpty,
              graphs.values.forall(_.directiveDefinitions.map(_.name) == List("cacheTag"))
            )
        }
      },
      test("filters implements by graph") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |interface Node @join__type(graph: A) @join__type(graph: B) { id: ID! }
              |type Widget implements Node @join__type(graph: A) @join__type(graph: B)
              |  @join__implements(graph: A, interface: "Node") {
              |  id: ID!
              |}""".stripMargin
          )
        ).map { result =>
          def implemented(graphs: Map[String, Document], graph: String) =
            graphs(graph).objectTypeDefinitions.find(_.name == "Widget").map(_.implements.map(_.name))

          assertTrue(
            result.map(implemented(_, "a")) == Right(Some(List("Node"))),
            result.map(implemented(_, "b")) == Right(Some(Nil))
          )
        }
      },
      test("filters input object fields by graph") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |input Filter @join__type(graph: A) @join__type(graph: B) {
              |  onlyA: String @join__field(graph: A)
              |  shared: String
              |}
              |type Widget @join__type(graph: A) @join__type(graph: B) { size(filter: Filter): Int! }""".stripMargin
          )
        ).map { result =>
          def inputs(graphs: Map[String, Document], graph: String) =
            graphs(graph).inputObjectTypeDefinitions.find(_.name == "Filter").map(_.fields.map(_.name))

          assertTrue(
            result.map(inputs(_, "a")) == Right(Some(List("onlyA", "shared"))),
            result.map(inputs(_, "b")) == Right(Some(List("shared")))
          )
        }
      },
      test("names only the operation roots a graph actually populates") {
        decompose(
          supergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Mutation @join__type(graph: A) { save: Boolean! @join__field(graph: A) }
              |type Subscription @join__type(graph: B) { watch: Boolean! @join__field(graph: B) }""".stripMargin
          )
        ).map { result =>
          def roots(graphs: Map[String, Document], graph: String) =
            graphs(graph).schemaDefinition.map(d => (d.query, d.mutation, d.subscription))

          assertTrue(
            result.map(roots(_, "a")) == Right(Some((Some("Query"), Some("Mutation"), None))),
            result.map(roots(_, "b")) == Right(Some((Some("Query"), None, Some("Subscription"))))
          )
        }
      }
    ),

    // A supergraph does not carry a context argument on the field it belongs to: the composer
    // folds the argument, its type and its selection into `@join__field(contextArguments:)` and
    // deletes it, and namespaces every `@context` name by the subgraph that declared it. Both
    // halves have to be undone, and only for the one graph that owns them.
    suite("projection: contexts")(
      test("re-emits each graph's own context declarations under the name that graph wrote") {
        // `Character` carries both graphs' declarations side by side, distinguished only by the
        // namespace — which is the subgraph *name*, while the join enum key is `CHARACTERS`.
        contextProjected.map { graphs =>
          assertTrue(
            contextNames(graphs("characters"), "Character") == List("viewer"),
            contextNames(graphs("episodes"), "Character") == List("crew")
          )
        }
      },
      test("strips the context feature's own definitions and types") {
        contextProjected.map { graphs =>
          assertTrue(
            graphs.values.forall(_.directiveDefinitions.isEmpty),
            graphs.values.forall(document => !types(document).contains("context__ContextFieldValue"))
          )
        }
      },
      test("rebuilds every context argument the composer folded into the join metadata") {
        contextProjected.map { graphs =>
          val characters = graphs("characters")

          assertTrue(
            // `size` is the only argument the supergraph still declares; the other two are
            // appended, which the composed schema cannot see — it hides context arguments.
            arguments(characters, "Ship", "fare").map(_.name) == List("size", "currency", "locale"),
            arguments(characters, "Ship", "fare").map(_.ofType) ==
              List(NamedType("Int", nonNull = true), NamedType("String", false), NamedType("String", false)),
            arguments(characters, "Ship", "fare").forall(_.defaultValue.isEmpty),
            fromContext(characters, "Ship", "fare") ==
              List("currency" -> "$viewer { currency }", "locale" -> "$viewer { locale }")
          )
        }
      },
      test("reconstructs a selection carrying a type condition") {
        contextProjected.map { graphs =>
          assertTrue(
            fromContext(graphs("episodes"), "Ship", "manifest") ==
              List("rank" -> "$crew ... on Character { rank }")
          )
        }
      },
      test("gives a context argument only to the graph that declared the context") {
        contextProjected.map { graphs =>
          assertTrue(
            fields(graphs("characters"), "Ship") == List("id", "fare"),
            fields(graphs("episodes"), "Ship") == List("id", "manifest")
          )
        }
      },
      test("declares a context on an interface and a union, not only on an object") {
        decompose(
          contextSupergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |interface Node @join__type(graph: A) @join__type(graph: B)
              |  @context(name: "a__nodeContext") { id: ID! }
              |union Holder @join__type(graph: A) @join__type(graph: B)
              |  @context(name: "b__holderContext") = Widget
              |type Widget @join__type(graph: A) @join__type(graph: B) { size: Int! }""".stripMargin
          )
        ).map {
          case Left(errors)  => assertTrue(errors == Nil)
          case Right(graphs) =>
            assertTrue(
              contextNames(graphs("a"), "Node") == List("nodeContext"),
              contextNames(graphs("b"), "Node") == Nil,
              contextNames(graphs("b"), "Holder") == List("holderContext"),
              contextNames(graphs("a"), "Holder") == Nil
            )
        }
      },
      test("rejects a context argument naming a context the supergraph never declares") {
        decompose(
          contextSupergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: A) {
              |  size(unit: String): Int! @join__field(graph: A,
              |    contextArguments: [{context: "a__missing", name: "unit", type: "String", selection: " { unit }"}])
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(List("[supergraph] Field 'Widget.size' names the undeclared context 'a__missing'."))
          )
        )
      },
      test("rejects a context argument naming another graph's context") {
        // Federation requires @context and @fromContext in the same subgraph, so this would
        // project an argument no subgraph can resolve.
        decompose(
          contextSupergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Holder @join__type(graph: B) @context(name: "b__viewer") { id: ID! }
              |type Widget @join__type(graph: A) {
              |  size(unit: String): Int! @join__field(graph: A,
              |    contextArguments: [{context: "b__viewer", name: "unit", type: "String", selection: " { id }"}])
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] Field 'Widget.size' names context 'b__viewer', which graph 'a' does not declare.")
            )
          )
        )
      },
      test("rejects a context declared for a graph that does not declare the type") {
        // The declaration can be projected nowhere — graph `b` never receives `Holder` — so
        // without this the field entry below looks consistent and the failure lands on the
        // projection instead, as `context 'viewer' is not declared by this subgraph`.
        decompose(
          contextSupergraph(
            """enum join__Graph {
              |  A @join__graph(name: "a", url: "http://a/graphql")
              |  B @join__graph(name: "b", url: "http://b/graphql")
              |}
              |type Holder @join__type(graph: A) @context(name: "b__viewer") { id: ID! }
              |type Widget @join__type(graph: B) {
              |  size(unit: String): Int! @join__field(graph: B,
              |    contextArguments: [{context: "b__viewer", name: "unit", type: "String", selection: " { id }"}])
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List(
                "[supergraph] Type 'Holder' declares context 'b__viewer' for graph 'b', which does not declare the type."
              )
            )
          )
        )
      },
      test("rejects an empty context argument selection") {
        decompose(
          contextSupergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: A) @context(name: "a__viewer") {
              |  size(unit: String): Int! @join__field(graph: A,
              |    contextArguments: [{context: "a__viewer", name: "unit", type: "String", selection: "  "}])
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List(
                "[supergraph] Field 'Widget.size' declares an empty context argument selection for context 'a__viewer'."
              )
            )
          )
        )
      },
      test("rejects an unparseable context argument type") {
        decompose(
          contextSupergraph(
            """enum join__Graph { A @join__graph(name: "a", url: "http://a/graphql") }
              |type Widget @join__type(graph: A) @context(name: "a__viewer") {
              |  size(unit: String): Int! @join__field(graph: A,
              |    contextArguments: [{context: "a__viewer", name: "unit", type: "[String", selection: " { unit }"}])
              |}""".stripMargin
          )
        ).map(result =>
          assertTrue(
            result == Left(
              List("[supergraph] Field 'Widget.size' declares the unparseable context argument type '[String'.")
            )
          )
        )
      }
    ),

    // ---------------------------------------------------------------------------------------
    // Task 9 — the Hive-composed fixture
    //
    // Hive composes with `@theguild/federation-composition`, not rover, so its supergraph is a
    // second dialect of the same artifact: a different definition order, different argument
    // printing, and two join definitions rover emits that it does not. None of that may reach the
    // projection. If this suite ever goes red, the reported difference is the specification for
    // whatever `SupergraphDecomposition` is missing — file it rather than relaxing the assertion.
    // ---------------------------------------------------------------------------------------
    suite("hive-composed fixture")(
      test("resolves the same subgraph registry, urls included") {
        // The urls are the half a composition invocation gets wrong silently: `composeServices`
        // defaults a missing one to the empty string instead of refusing, unlike rover.
        hiveFixtureSdl.flatMap(graphs).map { result =>
          assertTrue(
            entries(result) == Right(
              List(
                "characters" -> "http://127.0.0.1:9008/graphql/federated",
                "episodes"   -> "http://127.0.0.1:9009/graphql/federated"
              )
            )
          )
        }
      },
      test("projects to the same graph set as the rover-composed fixture") {
        for {
          rover <- projected
          hive  <- projectionOf(hiveFixtureSdl)
        } yield assertTrue(
          hive.keySet == rover.keySet,
          missing(rover("characters"), hive("characters")) == Nil,
          missing(hive("characters"), rover("characters")) == Nil,
          missing(rover("episodes"), hive("episodes")) == Nil,
          missing(hive("episodes"), rover("episodes")) == Nil
        )
      },
      test("the two fixtures are genuinely different documents, so the comparison proves something") {
        // Guards the suite above against a fixture accidentally regenerated from rover: identical
        // inputs would make every assertion in it vacuous.
        for {
          rover <- fixtureSdl
          hive  <- hiveFixtureSdl
        } yield assertTrue(rover != hive)
      }
    )
  )
}
