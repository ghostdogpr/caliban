package caliban.execution

import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.{ ResponseValue, RootResolver }
import caliban.TestUtils.Origin.{ BELT, EARTH, MARS }
import caliban.TestUtils.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.TestUtils.{ characters, resolverIO, CaptainShipName, Character, CharacterArgs, Origin, Role }
import caliban.schema.Annotations.GQLName
import caliban.schema.GenericSchema
import caliban.wrappers.DeferSupport
import zio.{ Has, UIO, URIO, ZIO, ZLayer }
import zio.stream.ZStream
import zio.test.Assertion.{ equalTo, hasSameElements }
import zio.test.{ assert, assertCompletesM, assertM, assertTrue, DefaultRunnableSpec, ZSpec }

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

  sealed trait By

  object By {
    case object Origin extends By
    case object Ship   extends By
  }

  case class ConnectionArgs(by: By)

  @GQLName("Character")
  case class CharacterZIO(
    name: String,
    nicknames: UIO[List[UIO[String]]],
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
      nicknames = UIO.succeed(ch.nicknames.map(UIO.succeed(_))),
      origin = ch.origin,
      role = ch.role,
      connections = {
        case ConnectionArgs(By.Origin) =>
          ZIO
            .serviceWith[CharacterService](_.characterBy(_.origin == ch.origin))
            .map(_.filter(_ != ch).map(character2CharacterZIO))
        case ConnectionArgs(By.Ship)   =>
          val maybeShip = ch.role.collect {
            case Captain(CaptainShipName(shipName)) => shipName
            case Pilot(shipName)                    => shipName
            case Engineer(shipName)                 => shipName
            case Mechanic(shipName)                 => shipName
          }
          ZIO.serviceWith[CharacterService](_.characterBy(_.role.exists {
            case Captain(CaptainShipName(shipName)) => maybeShip.contains(shipName)
            case Pilot(shipName)                    => maybeShip.contains(shipName)
            case Engineer(shipName)                 => maybeShip.contains(shipName)
            case Mechanic(shipName)                 => maybeShip.contains(shipName)
          }).map(_.filter(_ != ch).map(character2CharacterZIO)))
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

  val interpreter = (graphQL(resolver) @@ DeferSupport.deferSupport @@ DeferSupport.streamSupport).interpreter

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Defer Execution")(
    testM("don't defer pure fields") {

      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer {
               name
             }
           }
        }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(rest.isEmpty) && assertTrue(
        first.data.toString == """{"character":{"name":"Roberta Draper"}}"""
      )
    },
    testM("inline fragments") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer {
               name
               nicknames
             }
           }
        }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(
        first.data.toString == """{"character":{"name":"Roberta Draper"}}"""
      ) && assertTrue(
        rest.toList.map(_.toString) == List(
          """{"data":{"nicknames":["Bobbie","Gunny"]},"hasNext":false,"path":["character"]}"""
        )
      )
    },
    testM("named fragments") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ...Fragment @defer
           }
        }
        
        fragment Fragment on Character {
          name
          nicknames
        }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(
        first.data.toString == """{"character":{"name":"Roberta Draper"}}"""
      ) && assertTrue(
        rest.toList.map(_.toString) == List(
          """{"data":{"nicknames":["Bobbie","Gunny"]},"hasNext":false,"path":["character"]}"""
        )
      )
    },
    testM("disable") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer(if: false, label: "first") { nicknames }
           }
        }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(
        first.data.toString == """{"character":{"nicknames":["Bobbie","Gunny"]}}"""
      ) && assertTrue(rest.isEmpty)
    },
    testM("different labels") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer(label: "first") { n1: nicknames }
             ... @defer(label: "second") { n2: nicknames }
           }
        }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(
        first.data.toString == """{"character":{}}"""
      ) && assert(rest.toList.map(_.toString))(
        hasSameElements(
          List(
            """{"data":{"n1":["Bobbie","Gunny"]},"label":"first","hasNext":true,"path":["character"]}""",
            """{"data":{"n2":["Bobbie","Gunny"]},"label":"second","hasNext":false,"path":["character"]}"""
          )
        )
      )
    },
    testM("nested defers") {
      val query = gqldoc("""
           query test {
             character(name: "Roberta Draper") {
               name
               ... @defer(label: "outer") { 
                  connections(by: Origin) {
                    ...FragmentHuman @defer(label: "inner")
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
          """{"data":{"connections":[{"name":"Alex Kamal"}]},"label":"outer","hasNext":true,"path":["character"]}""",
          """{"data":{"nicknames":[]},"label":"inner","hasNext":false,"path":["character","connections",0]}"""
        )
      )
    },
    testM("streaming values") {
      val query = gqldoc("""
           query test {
            character(name: "Roberta Draper") {
               name
               nicknames @stream(label: "nicknames", initialCount: 1)
            }
           }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- DeferredGraphQLResponse(first).tail.runCollect
      } yield assertTrue(
        first.data.toString == """{"character":{"name":"Roberta Draper","nicknames":["Bobbie",null]}}"""
      ) && assertTrue(
        rest.toList.map(_.toString) == List(
          """{"data":"Gunny","label":"nicknames","hasNext":false,"path":["character","nicknames",1]}"""
        )
      )
    }
  ).provideCustomLayer(CharacterService.test)
}
