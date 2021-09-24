package poc.caliban.app

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.SelectionBuilder
import poc.caliban.client.generated.posts.CalibanClient._
import poc.caliban.posts.PostService
import sttp.capabilities
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import sttp.model.Uri
import zio.magic._
import zio.test.Assertion._
import zio.test.TestAspect.sequential
import zio.test._
import zio.test.environment.TestEnvironment
import zio.{Managed, Task, ZIO}

import java.util.UUID

object ServerSpec extends DefaultRunnableSpec {
  import sttp.client3.httpclient.zio.HttpClientZioBackend

  private val apiUrl: Uri = Uri.parse("http://localhost:8080/api/graphql").toOption.get

  def withServer(
                  test: SttpBackend[Task, ZioStreams with WebSockets] => ZIO[TestEnvironment, Throwable, TestResult]
                ): ZIO[TestEnvironment, Throwable, TestResult] =
    (for {
      backend: SttpBackend[Task, ZioStreams with capabilities.WebSockets] <- HttpClientZioBackend.managed()
      _                                                                   <- Main.server
      response                                                            <- Managed.fromEffect(test(backend))
    } yield response).useNow.injectSome[TestEnvironment](PostService.layer)

  val createMillPostMutation: SelectionBuilder[RootMutation, Option[String]] =
    Mutation
      .createPost(
        AuthorNameInput("John Stuart Mill"),
        PostTitleInput("Utilitarianism"),
        PostContentInput(
          "It is better to be a human being dissatisfied than a pig satisfied; better to be Socrates dissatisfied than a fool satisfied. And if the fool, or the pig, is of a different opinion, it is only because they only know their own side of the question."
        )
      )(Post.id(PostId.id))

  private val calibanServerSpec                                              =
    suite("Caliban server Spec")(
      test("truthiness")(assert(true)(isTrue)),
      testM("Create a post returns a 200") {
        withServer { backend =>
          val response = createMillPostMutation.toRequest(apiUrl).send(backend)
          assertM(response.map(_.code.code))(equalTo(200))
        }
      },
      testM("Fetch a non existing post returns None") {
        withServer { backend =>
          val query: SelectionBuilder[RootQuery, Option[String]] = Query.postById(UUID.randomUUID().toString)(Post.id(PostId.id))

          val response = query.toRequest(apiUrl).send(backend)

          assertM(response.map(_.body).absolve)(isNone)
        }
      },
      testM("Fetch an existing post returns Some(_)") {
        withServer { backend =>
          def query(id: String): SelectionBuilder[RootQuery, Option[String]] = Query.postById(id)(Post.author(AuthorName.name))

          val result: ZIO[TestEnvironment, Throwable, Option[String]] =
            for {
              id: String <- createMillPostMutation.toRequest(apiUrl).send(backend).map(_.body).absolve.map(_.get)
              author     <- query(id).toRequest(apiUrl).send(backend).map(_.body).absolve
            } yield author

          assertM(result)(isSome(equalTo("John Stuart Mill")))
        }
      },
    ) @@ sequential

  override def spec: ZSpec[TestEnvironment, Any]                             =
    suite("Server Spec")(
      calibanServerSpec,
    )
}
