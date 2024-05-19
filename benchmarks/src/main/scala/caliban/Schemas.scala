package caliban

import caliban.Data._
import caliban.parsing.adt.Document
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import cats.effect.IO
import grackle.generic.GenericMapping
import io.circe.{ Encoder, Json }
import zio.{ Runtime, Task, UIO, Unsafe, ZIO }

import scala.concurrent.Future

object Caliban {
  import caliban.schema.Schema

  private val runtime = Runtime.default

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)

  case class Query(
    characters: CharactersArgs => UIO[List[Character]],
    character: CharacterArgs => UIO[Option[Character]]
  )

  implicit val originSchema: Schema[Any, Origin]       = Schema.gen
  implicit val characterSchema: Schema[Any, Character] = Schema.gen

  val resolver: RootResolver[Query, Unit, Unit] = RootResolver(
    Query(
      args => ZIO.succeed(Data.characters.filter(c => args.origin.forall(c.origin == _))),
      args => ZIO.succeed(Data.characters.find(c => c.name == args.name))
    )
  )

  val gql: GraphQL[Any]  = graphQL(resolver)
  val document: Document = gql.toDocument

  val interpreter: GraphQLInterpreter[Any, CalibanError] = run(gql.interpreter)

  def run[A](zio: Task[A]): A = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())
}

object Sangria {
  import sangria.macros.derive._
  import sangria.schema._

  implicit val OriginEnum: EnumType[Origin]                  = deriveEnumType[Origin](IncludeValues("EARTH", "MARS", "BELT"))
  implicit val CaptainType: ObjectType[Unit, Role.Captain]   = deriveObjectType[Unit, Role.Captain]()
  implicit val PilotType: ObjectType[Unit, Role.Pilot]       = deriveObjectType[Unit, Role.Pilot]()
  implicit val EngineerType: ObjectType[Unit, Role.Engineer] = deriveObjectType[Unit, Role.Engineer]()
  implicit val MechanicType: ObjectType[Unit, Role.Mechanic] = deriveObjectType[Unit, Role.Mechanic]()
  implicit val RoleType: UnionType[Unit]                     = UnionType(
    "Role",
    types = List(PilotType, EngineerType, MechanicType, CaptainType)
  )
  implicit val CharacterType: ObjectType[Unit, Character]    = ObjectType(
    "Character",
    fields[Unit, Character](
      Field(
        "name",
        StringType,
        resolve = _.value.name
      ),
      Field(
        "nicknames",
        ListType(StringType),
        resolve = _.value.nicknames
      ),
      Field(
        "origin",
        OriginEnum,
        resolve = _.value.origin
      ),
      Field(
        "role",
        OptionType(RoleType),
        resolve = _.value.role
      )
    )
  )

  val OriginArg: Argument[Option[Origin]] = Argument("origin", OptionInputType(OriginEnum))
  val NameArg: Argument[String]           = Argument("name", StringType)

  val QueryType: ObjectType[Unit, Unit] = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "characters",
        ListType(CharacterType),
        arguments = OriginArg :: Nil,
        resolve = args => Future.successful(Data.characters.filter(c => (args arg OriginArg).forall(c.origin == _)))
      ),
      Field(
        "character",
        OptionType(CharacterType),
        arguments = NameArg :: Nil,
        resolve = args => Future.successful(Data.characters.find(c => c.name == (args arg NameArg)))
      )
    )
  )

  val schema: Schema[Unit, Unit] = Schema(QueryType)

}

object Grackle extends GenericMapping[IO] {
  import grackle.Predicate._
  import grackle.Query._
  import grackle.QueryCompiler._
  import grackle.Value._
  import grackle._
  import grackle.generic._
  import grackle.syntax._
  import semiauto._

  val schema =
    schema"""
          type Query {
              character(id: String!): Character
              characters(origin: Origin): [Character!]!
            }
          enum Origin {
            EARTH
            MARS
            BELT
          }
          type Captain {
            shipName: String!
          }
          type Pilot {
            shipName: String!
          }
          type Engineer {
            shipName: String!
          }
          type Mechanic {
            shipName: String!
          }
          union Role = Captain | Pilot | Engineer | Mechanic
          type Character {
            name: String!
            nicknames: [String!]!
            origin: Origin!
            role: Role
          }
        """

  val QueryType     = schema.ref("Query")
  val OriginType    = schema.ref("Origin")
  val CharacterType = schema.ref("Character")
  val CaptainType   = schema.ref("Captain")
  val PilotType     = schema.ref("Pilot")
  val EngineerType  = schema.ref("Engineer")
  val RoleType      = schema.ref("Role")
  val MechanicType  = schema.ref("Mechanic")

  implicit val earthCursorBuilder: CursorBuilder[Origin] = {
    case class OriginCursor(context: Context, focus: Origin, parent: Option[Cursor], env: Env)
        extends PrimitiveCursor[Origin] {
      def withEnv(env0: Env): Cursor    = copy(env = env.add(env0))
      override def asLeaf: Result[Json] = Json.fromString(focus.toString).success
    }
    new CursorBuilder[Origin]           {
      val tpe = EnumType("Origin", None, List("EARTH", "MARS", "BELT").map(EnumValueDefinition(_, None, Nil)), Nil)

      def build(context: Context, focus: Origin, parent: Option[Cursor], env: Env): Result[Cursor] =
        OriginCursor(context.asType(tpe), focus, parent, env).success
    }
  }
  implicit val roleEncoder: Encoder[Role]                = Encoder.encodeString.contramap(_.toString)

  implicit val captainCursorBuiler: CursorBuilder[Role.Captain]   =
    deriveObjectCursorBuilder[Role.Captain](CaptainType)
  implicit val pilotCursorBuiler: CursorBuilder[Role.Pilot]       =
    deriveObjectCursorBuilder[Role.Pilot](PilotType)
  implicit val engineerCursorBuiler: CursorBuilder[Role.Engineer] =
    deriveObjectCursorBuilder[Role.Engineer](EngineerType)
  implicit val mechanicCursorBuiler: CursorBuilder[Role.Mechanic] =
    deriveObjectCursorBuilder[Role.Mechanic](MechanicType)
  implicit val roleCursorBuilder: CursorBuilder[Role]             =
    CursorBuilder.deriveLeafCursorBuilder[Role](RoleType)
  implicit val characterCursorBuiler: CursorBuilder[Character]    =
    deriveObjectCursorBuilder[Character](CharacterType)

  val typeMappings =
    List(
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          GenericField("character", Data.characters),
          GenericField("characters", Data.characters)
        )
      )
    )

  implicit val eq: cats.Eq[Origin] = cats.Eq.fromUniversalEquals[Origin]

  // #elaborator
  override val selectElaborator = SelectElaborator {
    case (QueryType, f @ "character", List(Binding("id", IDValue(id))))      =>
      Elab.transformChild { child =>
        Unique(Filter(Eql(CharacterType / "id", Const(id)), child))
      }
    case (QueryType, "characters", List(Binding("origin", EnumValue(name)))) =>
      val originOpt = name match {
        case "EARTH" => Some(Origin.EARTH)
        case "MARS"  => Some(Origin.MARS)
        case "BELT"  => Some(Origin.BELT)
        case _       => None
      }
      originOpt
        .map((origin: Origin) =>
          Elab.transformChild { child =>
            Unique(Filter(Eql(CharacterType / "origin", Const(origin)), child))
          }
        )
        .getOrElse(Elab.failure(s"Unknown origin '$name'"))
  }

  def run[A](io: IO[A]): A = io.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
}

object Gql {
  import cats.effect._
  import cats.implicits._
  import gql._
  import gql.ast._
  import gql.dsl.all._

  implicit val origin: Enum[Origin] = enumType[Origin](
    "Origin",
    "EARTH" -> enumVal(Origin.EARTH),
    "MARS"  -> enumVal(Origin.MARS),
    "BELT"  -> enumVal(Origin.BELT)
  )

  implicit lazy val character: Type[IO, Character] = tpe[IO, Character](
    "Character",
    "name"      -> lift(_.name),
    "nicknames" -> lift(_.nicknames),
    "origin"    -> lift(_.origin),
    "role"      -> lift(_.role)
  )

  implicit lazy val captain: Type[IO, Role.Captain]   = tpe[IO, Role.Captain](
    "Captain",
    "shipName" -> lift(_.shipName)
  )
  implicit lazy val pilot: Type[IO, Role.Pilot]       = tpe[IO, Role.Pilot](
    "Pilot",
    "shipName" -> lift(_.shipName)
  )
  implicit lazy val engineer: Type[IO, Role.Engineer] = tpe[IO, Role.Engineer](
    "Engineer",
    "shipName" -> lift(_.shipName)
  )
  implicit lazy val mechanic: Type[IO, Role.Mechanic] = tpe[IO, Role.Mechanic](
    "Mechanic",
    "shipName" -> lift(_.shipName)
  )

  implicit lazy val role: Union[IO, Role] = union[IO, Role]("Role").variant { case x: Role.Captain => x }.variant {
    case x: Role.Pilot => x
  }.variant { case x: Role.Engineer => x }.variant { case x: Role.Mechanic => x }

  val originArg  = arg[Option[Origin]]("origin")
  val nameArg    = arg[String]("name")
  val makeSchema = Schema.query(
    tpe[IO, Unit](
      "Query",
      "characters" -> build.from(
        arged(originArg).evalMap(origin => IO.pure(Data.characters.filter(c => origin.forall(c.origin == _))))
      ),
      "character"  -> build.from(arged(nameArg).evalMap(name => IO.pure(Data.characters.find(c => c.name == name))))
    )
  )
  val schema     = run(makeSchema)

  def run[A](io: IO[A]): A = io.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
}
