package caliban.interop.tapir

import caliban.GraphQL.graphQL
import caliban.interop.tapir.TestData._
import caliban.interop.tapir.TestService.TestService
import caliban.{ GraphQL, RootResolver }
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.{ GenericSchema, Schema }
import caliban.uploads.{ Upload, Uploads }
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers._
import zio.{ URIO, ZIO }
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.stream.ZStream

import scala.language.postfixOps

object TestApi extends GenericSchema[TestService with Uploads] {

  case class File(hash: String, filename: String, mimetype: String)
  case class UploadFileArgs(file: Upload)
  case class UploadFilesArgs(files: List[Upload])

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[TestService, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[TestService, Option[Character]]
  )
  case class Mutations(
    deleteCharacter: CharacterArgs => URIO[TestService, Boolean],
    uploadFile: UploadFileArgs => ZIO[Uploads, Throwable, File],
    uploadFiles: UploadFilesArgs => ZIO[Uploads, Throwable, List[File]]
  )
  case class Subscriptions(characterDeleted: ZStream[TestService, Nothing, String])

  implicit val roleSchema: Schema[Any, Role]                     = Schema.gen
  implicit val characterSchema: Schema[Any, Character]           = Schema.gen
  implicit val characterArgsSchema: Schema[Any, CharacterArgs]   = Schema.gen
  implicit val charactersArgsSchema: Schema[Any, CharactersArgs] = Schema.gen

  val api: GraphQL[Console with Clock with TestService with Uploads] =
    graphQL(
      RootResolver(
        Queries(
          args => TestService.getCharacters(args.origin),
          args => TestService.findCharacter(args.name)
        ),
        Mutations(
          args => TestService.deleteCharacter(args.name),
          args => TestService.uploadFile(args.file),
          args => TestService.uploadFiles(args.files)
        ),
        Subscriptions(TestService.deletedEvents)
      )
    ) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      printErrors @@                  // wrapper that logs errors
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing

}
