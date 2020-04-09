package caliban

import caliban.ExampleData.Origin.{ BELT, EARTH, MARS }
import caliban.ExampleData.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.ExampleService.ExampleService
import caliban.schema.Annotations.GQLDirective
import zquery.ZQuery

object ExampleData {

  sealed trait Origin

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
  }

  sealed trait Role

  object Role {
    case class Captain(shipName: String)  extends Role
    case class Pilot(shipName: String)    extends Role
    case class Engineer(shipName: String) extends Role
    case class Mechanic(shipName: String) extends Role
  }

  @GQLDirective(federation.Key("name"))
  case class Character(name: String,
                       nicknames: List[String],
                       origin: Origin,
                       role: Option[Role],
                       starredIn: List[Episode] = Nil)

  @GQLDirective(federation.Key("season episode"))
  @GQLDirective(federation.Extend)
  case class Episode(
    @GQLDirective(federation.External) season: Int,
    @GQLDirective(federation.External) episode: Int,
    characters: ZQuery[ExampleService, Nothing, List[Character]] = ZQuery.succeed(List.empty)
  )

  def queryCharacters(season: Int, episode: Int): ZQuery[ExampleService, Nothing, List[Character]] =
    ZQuery.fromEffect(ExampleService.getCharactersByEpisode(season, episode))

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)
  case class EpisodeArgs(season: Int, episode: Int)

  lazy val sampleCharacters = List(
    Character("James Holden",
              List("Jim", "Hoss"),
              EARTH,
              Some(Captain("Rocinante")),
              List(Episode(1, 1), Episode(1, 2), Episode(2, 1))),
    Character("Naomi Nagata",
              Nil,
              BELT,
              Some(Engineer("Rocinante")),
              List(Episode(1, 1), Episode(1, 2), Episode(2, 1))),
    Character("Amos Burton",
              Nil,
              EARTH,
              Some(Mechanic("Rocinante")),
              List(Episode(1, 1), Episode(1, 2), Episode(2, 1))),
    Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante")), List(Episode(1, 1), Episode(1, 2), Episode(2, 1))),
    Character("Chrisjen Avasarala", Nil, EARTH, None, List(Episode(1, 1), Episode(1, 2), Episode(2, 1))),
    Character("Josephus Miller", List("Joe"), BELT, None, List(Episode(1, 1), Episode(1, 2), Episode(2, 1))),
    Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, None, List(Episode(2, 1)))
  ).map { c =>
    c.copy(starredIn = c.starredIn.map(e => e.copy(characters = queryCharacters(e.season, e.episode))))
  }

}
