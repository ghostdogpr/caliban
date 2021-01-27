package caliban.federation.tracing

import caliban.Macros.gqldoc
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.{ GraphQL, GraphQLRequest, RootResolver }
import caliban.schema.GenericSchema
import mdg.engine.proto.reports.Trace
import zio.{ UIO, URIO }
import zio.clock.Clock
import zio.duration.durationInt
import zio.query.ZQuery
import zio.test.Assertion._
import zio.test.TestAspect.{ flaky, timed }
import zio.test.environment.TestClock
import zio.test._
import com.google.protobuf.timestamp.Timestamp
import mdg.engine.proto.reports.Trace.Node
import mdg.engine.proto.reports.Trace.Node.Id.{ Index, ResponseName }

import java.util.Base64

object FederationTracingSpec extends DefaultRunnableSpec with GenericSchema[Clock] {

  case class Parent(name: String)
  case class Name(first: String, last: Option[String])

  case class User(
    id: String,
    username: URIO[Clock, Name],
    age: Int,
    parents: UIO[List[Parent]]
  )

  case class Queries(me: ZQuery[Any, Nothing, User])

  val api: GraphQL[Clock] = GraphQL.graphQL(
    RootResolver(
      Queries(
        me = ZQuery.succeed(
          User(
            "abc123",
            URIO.sleep(100.millis) as Name("my_first", Some("my_last")),
            age = 42,
            parents = UIO(List(Parent("my_parent")))
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

  // format: off
  val expectedTrace = Trace(
    Some(Timestamp(1,0)),
    Some(Timestamp(1,100000000)),
    100000000,
    Some(
      Node(
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
      )
    )
  )
  // format: on

  def parseTrace(trace: String) = Trace.parseFrom(Base64.getDecoder.decode(trace))

  override def spec =
    suite("Federation Tracing")(
      testM("disabled by default") {
        for {
          _           <- TestClock.setTime(1.second)
          interpreter <- api.interpreter
          resultFiber <- interpreter.execute(query).fork
          result      <- TestClock.adjust(1.second) *> resultFiber.join
        } yield assert(result.data)(equalTo(body)) && assert(
          result.extensions
        )(
          isNone
        )
      },
      testM("enabled") {
        for {
          _              <- TestClock.setTime(1.second)
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
        } yield assert(actualBody)(equalTo(body)) && assert(actualExtension)(
          isSome(
            equalTo(
              expectedTrace
            )
          )
        )
      }
    ) @@ flaky @@ timed
}
