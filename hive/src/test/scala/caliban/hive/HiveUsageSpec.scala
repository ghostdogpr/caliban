package caliban.hive

import caliban.CalibanError.ValidationError
import caliban.Value._
import caliban._
import caliban.execution.{ ExecutionRequest, Field }
import caliban.hive.HiveUsage.ClientInfo
import caliban.hive.internal.{ Operations, Record, Report }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.wrappers.Wrapper.ValidationWrapper
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import zio._
import zio.Config.Secret
import zio.http._
import zio.test._

object HiveUsageSpec extends ZIOSpecDefault {

  sealed trait Kind
  object Kind {
    case object ALBUM extends Kind
    case object TRACK extends Kind
  }
  final case class Filter(kind: Kind, minYear: Option[Int])
  final case class SearchArgs(query: String, filter: Option[Filter], limit: Option[Int])
  final case class ItemArgs(id: String)
  final case class Item(id: String, name: String, rating: Double)
  final case class Queries(search: SearchArgs => List[Item], item: ItemArgs => Option[Item])

  private val api = graphQL(
    RootResolver(Queries(_ => List(Item("1", "One", 4.5)), args => Some(Item(args.id, "One", 4.5))))
  )

  private def document(query: String): Document =
    Parser.parseQuery(query).fold(e => throw new IllegalArgumentException(e.msg), identity)

  /** The typed field tree Caliban validated for `query`, as [[HiveUsage.wrapper]] sees it. */
  private def fieldOf(query: String, variables: Map[String, InputValue] = Map.empty): Task[Field] =
    for {
      captured    <- Ref.make(Option.empty[Field])
      capture      = new ValidationWrapper[Any] {
                       def wrap[R1 <: Any](
                         f: Document => ZIO[R1, ValidationError, ExecutionRequest]
                       ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
                         doc => f(doc).tap(request => captured.set(Some(request.field)))
                     }
      interpreter <- (api @@ capture).interpreter
      response    <- interpreter.execute(query, variables = variables)
      _           <- ZIO.fail(new AssertionError(response.errors.mkString("; "))).when(response.errors.nonEmpty)
      field       <- captured.get.someOrFail(new AssertionError("validation did not run"))
    } yield field

  private def field(value: ResponseValue, name: String): ResponseValue = value match {
    case ResponseValue.ObjectValue(fields) => fields.toMap.getOrElse(name, NullValue)
    case _                                 => NullValue
  }

  private def list(value: ResponseValue): List[ResponseValue] = value match {
    case ResponseValue.ListValue(values) => values
    case _                               => Nil
  }

  private def record(key: String, subscription: Boolean = false, errors: Int = 0, client: Option[ClientInfo] = None) =
    Record(
      key = key,
      body = "{item(id:\"\"){id}}",
      name = None,
      coordinates = Set("Queries.item", "Queries.item.id", "Queries.item.id!", "String", "Item.id"),
      subscription = subscription,
      timestamp = 1700000000000L,
      duration = 1500000L,
      errors = errors,
      client = client
    )

  private def config(endpoint: URL) =
    HiveConfig(Secret("hive-token"), "org/project/target", endpoint, flushInterval = 1.hour)

  /** Runs `query` through an interpreter wrapped with [[HiveUsage.wrapper]], then releases the layer, which flushes. */
  private def report(config: HiveConfig, queries: (String, List[(String, String)])*) =
    ZIO.scoped(
      HiveUsage.layer(config).build.flatMap { env =>
        (api @@ HiveUsage.wrapper).interpreter.flatMap { interpreter =>
          ZIO.foreachDiscard(queries) { case (query, headers) =>
            IncomingRequestHeaders.locally(headers)(interpreter.execute(query)).provideEnvironment(env)
          }
        }
      }
    )

  def spec = suite("HiveUsage")(
    suite("normalize")(
      test("hides string, int and float literals, including nested and default values") {
        val body = Operations.normalize(
          document(
            """query Search($limit: Int = 25) {
              |  search(query: "secret user input", limit: 7, filter: { kind: ALBUM, minYear: 1999 }) { rating }
              |  item(id: "user-42") { name }
              |}""".stripMargin
          )
        )
        assertTrue(
          !body.contains("secret"),
          !body.contains("user-42"),
          !body.contains("25"),
          !body.contains("1999"),
          !body.contains("7"),
          body.contains("ALBUM")
        )
      },
      test("is the same for executions that differ only in literals, aliases and order") {
        val a =
          Operations.normalize(document("""{ x: item(id: "1") { name id } search(query: "a", limit: 1) { id } }"""))
        val b = Operations.normalize(document("""{ search(limit: 2, query: "b") { id } item(id: "2") { id name } }"""))
        assertTrue(a == b)
      },
      test("keeps variables, enum and boolean values, and the operation name") {
        val body =
          Operations.normalize(document("""query Find($id: String!) { item(id: $id) @include(if: true) { id } }"""))
        assertTrue(body.contains("Find"), body.contains("$id"), body.contains("true"))
      }
    ),
    test("name is the requested one, or that of the document's only operation") {
      val two = document("query A { item(id: \"1\") { id } } query B { item(id: \"2\") { id } }")
      assertTrue(
        Operations.name(document("query One { item(id: \"1\") { id } }"), None).contains("One"),
        Operations.name(document("{ item(id: \"1\") { id } }"), None).isEmpty,
        Operations.name(two, None).isEmpty,
        Operations.name(two, Some("B")).contains("B")
      )
    },
    suite("coordinates")(
      test("collects fields, arguments, input fields, enum values and scalars") {
        for {
          field <- fieldOf("""{ search(query: "q", filter: { kind: TRACK, minYear: 2000 }) { id rating } }""")
        } yield assertTrue(
          Operations.coordinates(field) == Set(
            "Queries.search",
            "Queries.search.query",
            "Queries.search.query!",
            "Queries.search.filter",
            "Queries.search.filter!",
            "String",
            "FilterInput.kind",
            "FilterInput.kind!",
            "Kind.TRACK",
            "FilterInput.minYear",
            "FilterInput.minYear!",
            "Int",
            "Item.id",
            "Item.rating"
          )
        )
      },
      test("resolves variables, and an explicit null counts as given but not as a value") {
        for {
          field      <- fieldOf(
                          """query($f: FilterInput, $l: Int) { search(query: "q", filter: $f, limit: $l) { id } }""",
                          Map("f" -> InputValue.ObjectValue(Map("kind" -> StringValue("ALBUM"))), "l" -> NullValue)
                        )
          coordinates = Operations.coordinates(field)
        } yield assertTrue(
          coordinates.contains("Kind.ALBUM"),
          coordinates.contains("FilterInput.kind!"),
          !coordinates.contains("FilterInput.minYear"),
          coordinates.contains("Queries.search.limit"),
          !coordinates.contains("Queries.search.limit!")
        )
      },
      test("leaves introspection out") {
        for {
          field <- fieldOf("""{ __typename item(id: "1") { __typename id } }""")
        } yield assertTrue(
          Operations.coordinates(field) ==
            Set("Queries.item", "Queries.item.id", "Queries.item.id!", "String", "Item.id")
        )
      }
    ),
    test("key separates executions with different coordinates or names") {
      val body = "{search(query:\"\"){id}}"
      assertTrue(
        Operations.key(body, None, Set("a", "b")) == Operations.key(body, None, Set("b", "a")),
        Operations.key(body, None, Set("a")) != Operations.key(body, None, Set("a", "b")),
        Operations.key(body, Some("A"), Set("a")) != Operations.key(body, None, Set("a"))
      )
    },
    test("client info needs both a name and a version header, in any case") {
      assertTrue(
        ClientInfo.fromHeaders(List("X-GraphQL-Client-Name" -> "web", "graphql-client-version" -> "1.2")) ==
          Some(ClientInfo("web", "1.2")),
        ClientInfo.fromHeaders(List("x-graphql-client-name" -> "web")).isEmpty
      )
    },
    test("report follows usage format v2") {
      val json       = readFromString[ResponseValue](
        Report.encode(
          Chunk(
            record("k1", client = Some(ClientInfo("web", "1.2"))),
            record("k1", errors = 2),
            record("k2", subscription = true)
          )
        )
      )
      val operations = list(field(json, "operations"))
      val executions = operations.map(field(_, "execution"))
      val map        = field(json, "map")
      assertTrue(
        field(json, "size") == IntValue(3),
        operations.map(field(_, "operationMapKey")) == List(StringValue("k1"), StringValue("k1")),
        executions.map(field(_, "ok")) == List(BooleanValue(true), BooleanValue(false)),
        executions.map(field(_, "errorsTotal")) == List(IntValue(0), IntValue(2)),
        executions.map(field(_, "duration")) == List(IntValue(1500000), IntValue(1500000)),
        operations
          .map(o => field(field(field(o, "metadata"), "client"), "name")) == List(StringValue("web"), NullValue),
        list(field(json, "subscriptionOperations")).map(field(_, "operationMapKey")) == List(StringValue("k2")),
        field(field(map, "k1"), "operation") == StringValue("{item(id:\"\"){id}}"),
        list(field(field(map, "k1"), "fields")).size == 5,
        field(field(map, "k1"), "operationName") == NullValue
      )
    },
    suite("layer")(
      test("sends normalized operations with token, API version and client when released") {
        for {
          endpoint       <- UsageEndpoint.start()
          (url, received) = endpoint
          _              <- report(
                              config(url),
                              """query Find { item(id: "user-42") { name } }""" ->
                                List("x-graphql-client-name" -> "web", "x-graphql-client-version" -> "1.2")
                            )
          requests       <- received.requests.get
          sent           <- received.bodies.get
          json            = sent.headOption.map(readFromString[ResponseValue](_)).getOrElse(NullValue)
          entries         = field(json, "map") match {
                              case ResponseValue.ObjectValue(fields) => fields.map(_._2)
                              case _                                 => Nil
                            }
        } yield assertTrue(
          requests.size == 1,
          requests.map(_.url.path.encode.split("/").toList.drop(2)) == Vector(List("org", "project", "target")),
          requests.flatMap(_.rawHeader("authorization")) == Vector("Bearer hive-token"),
          requests.flatMap(_.rawHeader("x-usage-api-version")) == Vector("2"),
          !sent.exists(_.contains("user-42")),
          entries.map(field(_, "operation")) == List(StringValue("query Find{item(id:\"\"){name}}")),
          entries.map(field(_, "operationName")) == List(StringValue("Find")),
          list(field(json, "operations")).map(o => field(field(o, "metadata"), "client")) ==
            List(ResponseValue.ObjectValue(List("name" -> StringValue("web"), "version" -> StringValue("1.2"))))
        )
      },
      test("leaves out excluded operations, sampled-out operations and pure introspection") {
        for {
          endpoint       <- UsageEndpoint.start()
          (url, received) = endpoint
          _              <- report(
                              config(url).copy(exclude = Set("Health")),
                              "query Health { item(id: \"1\") { id } }" -> Nil,
                              "{ __typename }"                          -> Nil,
                              "query Kept { item(id: \"1\") { id } }"   -> Nil
                            )
          _              <- report(config(url).copy(sampleRate = 0.0), "query Kept { item(id: \"1\") { id } }" -> Nil)
          sent           <- received.bodies.get
        } yield assertTrue(
          sent.size == 1,
          sent.forall(body => field(readFromString[ResponseValue](body), "size") == IntValue(1)),
          sent.forall(_.contains("Kept")),
          !sent.exists(_.contains("Health"))
        )
      },
      test("sends what does not fit one report in several") {
        for {
          endpoint       <- UsageEndpoint.start()
          (url, received) = endpoint
          query           = "query Kept { item(id: \"1\") { id } }" -> Nil
          _              <- report(config(url).copy(maxBatchSize = 2), query, query, query)
          sent           <- received.bodies.get
        } yield assertTrue(
          sent.map(body => field(readFromString[ResponseValue](body), "size")) == Vector(IntValue(2), IntValue(1))
        )
      },
      test("retries a report after a server error, but not one Hive rejects") {
        for {
          retried            <- UsageEndpoint.start(Status.ServiceUnavailable, Status.Ok)
          (retryUrl, first)   = retried
          rejected           <- UsageEndpoint.start(Status.Unauthorized)
          (rejectUrl, second) = rejected
          query               = "query Kept { item(id: \"1\") { id } }" -> Nil
          _                  <- report(config(retryUrl), query)
          _                  <- report(config(rejectUrl), query)
          retries            <- first.bodies.get
          rejections         <- second.bodies.get
          logs               <- ZTestLogger.logOutput
        } yield assertTrue(
          retries.size == 2,
          retries.distinct.size == 1,
          rejections.size == 1,
          logs.exists(_.message().startsWith("Hive rejected a usage report of 1 operations: 401"))
        )
      },
      test("drops and counts operations that do not fit the buffer") {
        for {
          endpoint       <- UsageEndpoint.start()
          (url, received) = endpoint
          query           = "query Kept { item(id: \"1\") { id } }" -> Nil
          _              <- report(config(url).copy(bufferSize = 1), query, query, query)
          sent           <- received.bodies.get
          logs           <- ZTestLogger.logOutput
        } yield assertTrue(
          sent.map(body => field(readFromString[ResponseValue](body), "size")) == Vector(IntValue(1)),
          logs.exists(_.message() == "Dropped 2 operations because the Hive usage buffer was full")
        )
      },
      test("refuses a batch or buffer size below 1") {
        for {
          exit <- ZIO.scoped(HiveUsage.layer(config(HiveConfig.cloudEndpoint).copy(maxBatchSize = 0)).build).exit
        } yield assertTrue(exit.isFailure)
      },
      test("sends on release what arrived while a report was in flight") {
        for {
          inFlight       <- Promise.make[Nothing, Unit]
          gate           <- Promise.make[Nothing, Unit]
          endpoint       <- UsageEndpoint.startWith(inFlight.succeed(()) *> gate.await)
          (url, received) = endpoint
          _              <- ZIO.scoped(
                              HiveUsage.layer(config(url).copy(flushInterval = 50.millis)).build.flatMap { env =>
                                (api @@ HiveUsage.wrapper).interpreter.flatMap { interpreter =>
                                  (for {
                                    _ <- interpreter.execute("query First { item(id: \"1\") { id } }")
                                    _ <- inFlight.await
                                    _ <- interpreter.execute("query Second { item(id: \"1\") { id } }")
                                    _ <- gate.succeed(()).delay(200.millis).forkDaemon
                                  } yield ()).provideEnvironment(env)
                                }
                              }
                            )
          sent           <- received.bodies.get
        } yield assertTrue(
          sent.size == 2,
          sent.headOption.exists(_.contains("First")),
          sent.lastOption.exists(_.contains("Second"))
        )
      }
    ).provide(Server.defaultWith(_.onAnyOpenPort), Client.default)
  ) @@ TestAspect.withLiveClock
}
