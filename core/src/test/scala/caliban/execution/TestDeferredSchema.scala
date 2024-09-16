package caliban.execution

import caliban.TestUtils.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.TestUtils.{ CaptainShipName, Character, CharacterArgs, Origin, Role }
import caliban.schema.Annotations.GQLName
import caliban.schema.{ GenericSchema, Schema, SchemaDerivation }
import caliban.wrappers.IncrementalDelivery
import caliban.{ graphQL, RootResolver }
import zio.query.{ DataSource, Request, UQuery, ZQuery }
import zio.stream.ZStream
import zio.{ UIO, ULayer, URIO, ZIO, ZLayer }

case class Episode(
  season: Int,
  episode: Int,
  title: String
)

trait QuoteService {
  def quotesBy(character: Character): ZStream[Any, Nothing, QuoteService.Quote]
}

object QuoteService {
  val test: ULayer[QuoteService] = ZLayer.succeed(new QuoteService {
    val quoteDB = Map(
      "Roberta Draper" -> List(
        Quote(
          "You just have to believe that what you’re doing really matters, and then the fear can’t control you.",
          from = Episode(4, 1, "New Terra")
        ),
        Quote(
          "I'm Roberta, you can call me Bobby.",
          from = Episode(4, 1, "New Terra")
        )
      )
    )

    override def quotesBy(character: Character): ZStream[Any, Nothing, Quote] =
      ZStream.fromIterable(quoteDB.getOrElse(character.name, Nil))

  })

  case class Quote(
    line: String,
    from: Episode
  )
}

object TestDeferredSchema extends SchemaDerivation[CharacterService with QuoteService] {
  import auto._
  import caliban.schema.ArgBuilder.auto._

  sealed trait By

  object By {
    case object Origin extends By

    case object Ship extends By
  }

  case class ConnectionArgs(by: By)

  case class Quote(
    line: String,
    from: Episode,
    character: UIO[CharacterZIO]
  )

  @GQLName("Character")
  case class CharacterZIO(
    name: String,
    nicknames: UIO[List[UIO[String]]],
    origin: Origin,
    role: Option[Role],
    connections: ConnectionArgs => URIO[CharacterService with QuoteService, List[CharacterZIO]],
    quotes: ZStream[CharacterService with QuoteService, Nothing, Quote] = ZStream.empty
  )

  case class Query(
    character: CharacterArgs => URIO[CharacterService with QuoteService, Option[CharacterZIO]]
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
      },
      quotes = ZStream.serviceWithStream[QuoteService](_.quotesBy(ch).map { q =>
        Quote(
          q.line,
          q.from,
          ZIO.succeed(character2CharacterZIO(ch))
        )
      })
    )

  implicit val characterArgsSchema: Schema[CharacterService with QuoteService, CharacterArgs] =
    genAll[CharacterService with QuoteService, CharacterArgs]
  implicit val roleSchema: Schema[CharacterService with QuoteService, Role]                   =
    gen[CharacterService with QuoteService, Role]
  implicit val originSchema: Schema[CharacterService with QuoteService, Origin]               =
    gen[CharacterService with QuoteService, Origin]
  implicit val episodeSchema: Schema[CharacterService with QuoteService, Episode]             =
    gen[CharacterService with QuoteService, Episode]
  implicit val quoteSchema: Schema[CharacterService with QuoteService, Quote]                 =
    gen[CharacterService with QuoteService, Quote]
  implicit lazy val characterSchema: Schema[CharacterService with QuoteService, CharacterZIO] =
    obj(
      "Character"
    )(implicit fa =>
      List(
        field("name")(_.name),
        field("nicknames")(_.nicknames),
        field("origin")(_.origin),
        field("role")(_.role),
        fieldWithArgs("connections")(_.connections),
        field("quotes")(_.quotes)
      )
    )
  implicit val querySchema: Schema[CharacterService with QuoteService, Query]                 =
    genAll[CharacterService with QuoteService, Query]

  val resolver =
    RootResolver(
      Query(
        character = args =>
          ZIO
            .serviceWithZIO[CharacterService](_.characterBy(_.name == args.name))
            .map(_.headOption.map(character2CharacterZIO))
      )
    )

  val interpreter = (graphQL(resolver) @@ IncrementalDelivery.all).interpreter
}

object TestDatasourceDeferredSchema extends GenericSchema[Any] {
  import auto._

  case class Bar(value: UQuery[String])
  case class Foo(bar: UIO[List[Bar]])
  case class Query(foo: UIO[Foo])

  case class Req(i: Int) extends Request[Nothing, String]
  private val ds = DataSource.fromFunctionZIO("ValuesDS")((_: Req) => ZIO.succeed("value"))

  private def makeBar(i: Int): Bar = Bar(ZQuery.fromRequest(Req(i))(ds))

  private val resolver = RootResolver(Query(ZIO.succeed {
    Foo(bar = ZIO.succeed(List(makeBar(1), makeBar(3), makeBar(3))))
  }))

  val interpreter = (graphQL(resolver) @@ IncrementalDelivery.defer).interpreter
}
