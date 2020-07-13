package caliban

import scala.concurrent.ExecutionContext.Implicits

import io.circe.Decoder
import org.http4s.{ Method, Uri }
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import zio._
import zio.interop.catz._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.multipart.Multipart
import zio.test.TestResult

object HttpClient {
  type HttpClient = Has[Service]

  trait Service {
    def get[T](uri: String, parameters: Map[String, String])(implicit d: Decoder[T]): Task[T]

    def post[T](uri: String, body: String, parameters: Map[String, String], multipart: Multipart[Task])(
      implicit d: Decoder[T]
    ): Task[T]
  }

  def make(host: String, port: Int = 80): ZLayer[Has[Client[Task]], Nothing, HttpClient] =
    ZLayer.fromService[Client[Task], Service](http4sClient => Http4s(host, port, http4sClient))

  def makeHttpClient: TaskManaged[Client[Task]] =
    ZIO.runtime[Any].toManaged_.flatMap { implicit rts =>
      BlazeClientBuilder
        .apply[Task](Implicits.global)
        .resource
        .toManaged
    }

  def testClientM[R](fClient: Client[Task] => Task[TestResult]): Task[TestResult] =
    ZIO.runtime[Any].flatMap { implicit rts =>
      val exec = rts.platform.executor.asEC
      BlazeClientBuilder[Task](exec).resource.use(client => fClient(client))
    }

  def get[T](resource: String, parameters: Map[String, String] = Map())(
    implicit d: Decoder[T]
  ): ZIO[HttpClient, Throwable, List[T]] =
    RIO.accessM[HttpClient](_.get.get[List[T]](resource, parameters))

  def post[T](resource: String, body: String, queryParameters: Map[String, String], multipart: Multipart[Task])(
    implicit d: Decoder[T]
  ): ZIO[HttpClient, Throwable, T] =
    RIO.accessM[HttpClient](_.get.post[T](resource, body, queryParameters, multipart))
}

final case class Http4s(host: String, port: Int, client: Client[Task])
    extends HttpClient.Service
    with Http4sClientDsl[Task] {
  val authority = Uri.Authority(host = Uri.RegName(host), port = Some(port))

  def get[T](resource: String, parameters: Map[String, String])(implicit d: Decoder[T]): Task[T] = {
    val uri = Uri(path = resource).withQueryParams(parameters)

    client.expect[T](uri.toString())
  }

  def post[T](resource: String, body: String, queryParameters: Map[String, String], multipart: Multipart[Task])(
    implicit d: Decoder[T]
  ): Task[T] = {
    val uri = Uri(authority = Some(authority), path = resource).withQueryParams(queryParameters)

    val request =
      Method.POST(multipart, uri).map(_.withHeaders(multipart.headers))

    client.expect[T](request)
  }
}
