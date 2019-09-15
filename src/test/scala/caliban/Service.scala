package caliban

import caliban.GraphQL._
import caliban.TestUtils.resolver
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio.{ Task, UIO, ZIO }
import zio.console.putStrLn
import zio.interop.catz._
import zio.interop.catz.implicits._

object Service extends CatsApp {

  object dsl extends Http4sDsl[Task]
  import dsl._

  val schema: GraphQL[TestUtils.Query, Unit, Unit] = graphQL(resolver)

  val graphQLService: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case req @ POST -> Root / "graphql" =>
      for {
        query    <- EntityDecoder.decodeString(req)
        result   <- schema.execute(query)
        response <- Ok(result.mkString)
      } yield response
  }

  val httpApp: HttpApp[Task] = Router("/api" -> CORS(graphQLService)).orNotFound
  val serverBuilder: BlazeServerBuilder[Task] =
    BlazeServerBuilder[Task].bindHttp(8080, "localhost").withHttpApp(httpApp)

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    serverBuilder.resource.toManaged[Any].useForever.foldM(err => putStrLn(err.toString).as(1), _ => UIO(0))
}
