package caliban.execution

import caliban.TestUtils.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.TestUtils.{ CaptainShipName, Character, CharacterArgs, Origin, Role }
import caliban.{ graphQL, RootResolver }
import caliban.schema.Annotations.GQLName
import caliban.schema.{ GenericSchema, Schema }
import caliban.wrappers.DeferSupport
import zio.{ UIO, URIO, ZIO }

object TestDeferredSchema extends GenericSchema[CharacterService] {
  import auto._
  import caliban.schema.ArgBuilder.auto._

  sealed trait By

  object By {
    case object Origin extends By

    case object Ship extends By
  }

  case class ConnectionArgs(by: By)

  @GQLName("Character")
  case class CharacterZIO(
    name: String,
    nicknames: UIO[List[UIO[String]]],
    origin: Origin,
    role: Option[Role],
    connections: ConnectionArgs => URIO[CharacterService, List[CharacterZIO]]
  )

  case class Query(
    character: CharacterArgs => URIO[CharacterService, Option[CharacterZIO]]
  )

  def character2CharacterZIO(ch: Character): CharacterZIO =
    CharacterZIO(
      name = ch.name,
      nicknames = ZIO.succeed(ch.nicknames.map(ZIO.succeed(_))),
      origin = ch.origin,
      role = ch.role,
      connections = {
        case ConnectionArgs(By.Origin) =>
          ZIO
            .serviceWithZIO[CharacterService](_.characterBy(_.origin == ch.origin))
            .map(_.filter(_ != ch).map(character2CharacterZIO))
        case ConnectionArgs(By.Ship)   =>
          val maybeShip = ch.role.collect {
            case Captain(CaptainShipName(shipName)) => shipName
            case Pilot(shipName)                    => shipName
            case Engineer(shipName)                 => shipName
            case Mechanic(shipName)                 => shipName
          }
          ZIO.serviceWithZIO[CharacterService](_.characterBy(_.role.exists {
            case Captain(CaptainShipName(shipName)) => maybeShip.contains(shipName)
            case Pilot(shipName)                    => maybeShip.contains(shipName)
            case Engineer(shipName)                 => maybeShip.contains(shipName)
            case Mechanic(shipName)                 => maybeShip.contains(shipName)
          }).map(_.filter(_ != ch).map(character2CharacterZIO)))
      }
    )

  implicit lazy val characterSchema: Schema[CharacterService, CharacterZIO] = genAll[CharacterService, CharacterZIO]
  implicit val querySchema: Schema[CharacterService, Query]                 = gen[CharacterService, Query]

  val resolver =
    RootResolver(
      Query(
        character = args =>
          ZIO
            .serviceWithZIO[CharacterService](_.characterBy(_.name == args.name))
            .map(_.headOption.map(character2CharacterZIO))
      )
    )

  val interpreter = (graphQL(resolver) @@ DeferSupport.defer @@ DeferSupport.stream).interpreter
}
