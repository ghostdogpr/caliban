package caliban.federation.tracing

import caliban.Macros.gqldoc
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban._
import caliban.schema.Schema.auto._
import mdg.engine.proto.reports.Trace
import zio._
import zio.query.ZQuery
import zio.test.Assertion._
import zio.test.TestAspect.{ flaky, timed }
import zio.test._
import com.google.protobuf.timestamp.Timestamp
import mdg.engine.proto.reports.Trace.Node
import mdg.engine.proto.reports.Trace.Node.Id
import mdg.engine.proto.reports.Trace.Node.Id.{ Index, ResponseName }

import java.util.Base64
import java.time.Instant

object FederationTracingSpec extends ZIOSpecDefault {

  case class Parent(name: String)
  case class Name(first: String, last: Option[String])

  case class User(
    id: String,
    username: UIO[Name],
    age: Int,
    parents: UIO[List[Parent]]
  )

  case class Queries(me: ZQuery[Any, Nothing, User])

  val api = graphQL(
    RootResolver(
      Queries(
        me = ZQuery.succeed(
          User(
            "abc123",
            ZIO.sleep(100.millis) as Name("my_first", Some("my_last")),
            age = 42,
            parents = ZIO.succeed(List(Parent("my_parent")))
          )
        )
      )
    )
  ) @@ ApolloFederatedTracing.wrapper

  val query = gqldoc("query { me { id username { first, family: last } parents { name } age } }")
  val body  = ObjectValue(
    List(
      "me" -> ObjectValue(
        List(
          "id"       -> StringValue("abc123"),
          "username" -> ObjectValue(
            List(
              "first"  -> StringValue("my_first"),
              "family" -> StringValue("my_last")
            )
          ),
          "parents"  -> ListValue(
            List(
              ObjectValue(
                List(
                  "name" -> StringValue("my_parent")
                )
              )
            )
          ),
          "age"      -> IntValue(42)
        )
      )
    )
  )

  def sortNode(node: Node): Node =
    node.copy(child =
      node.child
        .map(sortNode)
        .sortBy(_.id match {
          case Id.Empty            => ""
          case ResponseName(value) => value
          case Index(value)        => value.toString
        })
    )

  // format: off
  val expectedTrace = Trace(
    Some(Timestamp(1,0)),
    Some(Timestamp(1,100000000)),
    100000000,
    Some(
      sortNode(Node(
        Node.Id.Empty,
        child = Vector(
          Node(
            ResponseName("me"),"","User!","Queries",None,0,100000000,Vector(),
            Vector(
              Node(ResponseName("age"),"","Int!","User"),
              Node(ResponseName("id"),"","String!","User"),
              Node(ResponseName("parents"),"","[Parent!]!","User", child = Vector(
                Node(Index(0), child = Vector(Node(ResponseName("name"),"","String!","Parent")))
              )),
              Node(ResponseName("username"),"","Name!","User",None,0,100000000,Vector(),
                Vector(
                  Node(ResponseName("family"),"last","String","Name",None,100000000,100000000),
                  Node(ResponseName("first"),"","String!","Name",None,100000000,100000000)
                )
              )
            )
          )
        )
      ))
    )
  )
  // format: on

  def parseTrace(trace: String) = Trace.parseFrom(Base64.getDecoder.decode(trace))

  override def spec =
    suite("Federation Tracing")(
      test("disabled by default") {
        for {
          _           <- TestClock.setTime(Instant.ofEpochSecond(1))
          interpreter <- api.interpreter
          resultFiber <- interpreter.execute(query).fork
          result      <- TestClock.adjust(1.second) *> resultFiber.join
        } yield assertTrue(result.data == body) && assert(result.extensions)(isNone)
      },
      test("enabled") {
        for {
          _              <- TestClock.setTime(Instant.ofEpochSecond(1))
          interpreter    <- api.interpreter
          resultFiber    <-
            interpreter
              .execute(
                query,
                extensions = Map(GraphQLRequest.`apollo-federation-include-trace` -> StringValue(GraphQLRequest.ftv1))
              )
              .fork
          result         <- TestClock.adjust(1.second) *> resultFiber.join
          actualBody      = result.data
          actualExtension = result.extensions.flatMap(_.fields.collectFirst { case ("ftv1", StringValue(ftv1)) =>
                              parseTrace(ftv1)
                            })
          sortedTrace     = actualExtension.map(trace =>
                              trace.copy(
                                root = trace.root.map(sortNode)
                              )
                            )
        } yield assertTrue(actualBody == body) && assertTrue(sortedTrace.get == expectedTrace)
      }
    ) @@ flaky @@ timed
}
