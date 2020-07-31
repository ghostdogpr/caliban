package caliban.client.apq

import caliban.client.CalibanClientError.PersistedQueryNotFound
import caliban.client.TestData._
import caliban.client.apq.hash.CryptoHasher
import caliban.client.apq.helpers.RIOMonadAsyncError
import sttp.client._
import sttp.client.testing.SttpBackendStub
import zio.test.Assertion._
import zio.test._
import zio.{ test => _, _ }

object PersistedQuerySpec extends DefaultRunnableSpec {
  case class CharacterView(name: String, origin: Origin)

  val simpleQuery  = "{__typename}"
  val expectedHash = "ecf4edb46db40b5132295c0291d62fb65d6759a9eedfa4d5d612dd5ec54a6b38"
  val selection = Queries
    .character("Amos Burton") {
      Character.name ~ Character.origin
    }
    .map(_.map((CharacterView.apply _).tupled))

  val backendLayer: Layer[Nothing, Has[SttpBackend[Task, Nothing, NothingT]]] = ZLayer.succeed(
    SttpBackendStub[Task, Nothing, NothingT](new RIOMonadAsyncError).whenRequestMatchesPartial {
      case r if r.tag("counter").contains(0) =>
        Response.ok(Left(PersistedQueryNotFound))
      case r if r.tag("counter").contains(1) =>
        Response.ok(Right(Some(CharacterView("Amos Burton", Origin.BELT))))
    }
  )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("persisted queries")(
      test("sha256 hashing") {
        assert(CryptoHasher.Sha256(simpleQuery.getBytes("UTF-8")))(isRight(equalTo(expectedHash)))
      },
      testM("generated query") {
        for {
          ref <- Ref.make(0)
          result <- ZIO.service[SttpBackend[Task, Nothing, NothingT]].flatMap { backend =>
                     val b = SttpCounterBackend[Any, Nothing, NothingT](backend, ref)
                     assertM(PersistedQuery(selection).apply(uri"http://example.com/graphql")(b).map(_.body))(
                       isRight(equalTo(Some(CharacterView("Amos Burton", Origin.BELT))))
                     )
                   }
          calls <- ref.get
        } yield result && assert(calls)(equalTo(2))
      }.provideCustomLayer(backendLayer)
    )
}
