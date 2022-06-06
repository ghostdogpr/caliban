package caliban.execution

import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.{ ResponseValue, RootResolver }
import caliban.TestUtils.Origin.{ BELT, EARTH, MARS }
import caliban.TestUtils.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.TestUtils.{ characters, resolverIO, CaptainShipName, Character, CharacterArgs, Origin, Role }
import caliban.schema.Annotations.GQLName
import caliban.schema.GenericSchema
import zio.{ Has, UIO, URIO, ZIO, ZLayer }
import zio.stream.ZStream
import zio.test.Assertion.equalTo
import zio.test.{ assertM, assertTrue, DefaultRunnableSpec, ZSpec }

trait CharacterService {
  def characterBy(pred: Character => Boolean): UIO[List[Character]]
}

object CharacterService {
  val test = ZLayer.succeed(new CharacterService {
    override def characterBy(pred: Character => Boolean): UIO[List[Character]] =
      UIO.succeed(characters.filter(pred))
  })
}

object DeferredExecutionSpec extends DefaultRunnableSpec {

  object schema extends GenericSchema[Has[CharacterService]]
  import schema._

  case class ConnectionArgs(
    withOrigin: List[Origin] = Nil,
    withName: Option[String] = None
  )

  @GQLName("Character")
  case class CharacterZIO(
    name: String,
    nicknames: List[String],
    origin: Origin,
    role: Option[Role],
    connections: ConnectionArgs => URIO[Has[CharacterService], List[CharacterZIO]]
  )

  case class Query(
    character: CharacterArgs => URIO[Has[CharacterService], Option[CharacterZIO]]
  )

  def character2CharacterZIO(ch: Character): CharacterZIO =
    CharacterZIO(
      name = ch.name,
      nicknames = ch.nicknames,
      origin = ch.origin,
      role = ch.role,
      connections = { case ConnectionArgs(withOrigin, withName) =>
        ZIO
          .mapParN(
            ZIO.serviceWith[CharacterService](_.characterBy(c => withOrigin.contains(c.origin))),
            ZIO.serviceWith[CharacterService](_.characterBy(c => withName.contains(c.name)))
          )(_ ++ _)
          .map(_.distinct.filter(_.name != ch.name))
          .map(_.map(character2CharacterZIO))
      }
    )

  val resolver =
    RootResolver(
      Query(
        character = args =>
          ZIO
            .serviceWith[CharacterService](_.characterBy(_.name == args.name))
            .map(_.headOption.map(character2CharacterZIO))
      )
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Defer Execution")(
    testM("sanity") {
      val interpreter = graphQL(resolver).interpreter
      val query       = gqldoc("""
           query test {
             character(name: "Roberta Draper") {
               name
               ... @defer(label: "human") { 
                  nicknames
                  connections(withOrigin: [MARS, EARTH]) {
                    ...FragmentHuman @defer(label: "human")
                  }
               }
             }
           }
           
           fragment FragmentHuman on Character {
             name
             nicknames
           }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(first.data.toString == """{"character":{"name":"Roberta Draper"}}""") && assertTrue(
        rest.toList.map(_.toString) == List(
          """{"data":{"nicknames":["Bobbie","Gunny"]},"hasNext":false,"path":["character"]}"""
        )
      )
    }
  ).provideCustomLayer(CharacterService.test)
}
