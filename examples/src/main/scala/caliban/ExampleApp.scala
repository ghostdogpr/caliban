package caliban

import caliban.ExampleData._
import caliban.GraphQL._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import zio.{ UIO, ZIO }
import zio.console.putStrLn
import zio.interop.catz._
import zio.stream.ZStream

object ExampleApp extends CatsApp {

  case class Queries(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => UIO[Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => UIO[Boolean])
  case class Subscriptions(characterDeleted: ZStream[Any, Nothing, String])

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    (for {
      service <- ExampleService.make(sampleCharacters)
      interpreter = graphQL(
        RootResolver(
          Queries(args => service.getCharacters(args.origin), args => service.findCharacter(args.name)),
          Mutations(args => service.deleteCharacter(args.name)),
          Subscriptions(service.deletedEvents)
        )
      )
      _ <- Http4sAdapter.make(interpreter).useForever
    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
}
