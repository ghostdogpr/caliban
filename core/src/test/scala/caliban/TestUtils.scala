package caliban

import caliban.TestUtils.InvalidSchemas.Interface.WrongArgumentName
import caliban.TestUtils.Origin._
import caliban.TestUtils.Role._
import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLDirective, GQLInputName, GQLInterface }
import caliban.schema.Schema
import zio.UIO
import zio.stream.ZStream

object TestUtils {

  @GQLInterface
  sealed trait Interface
  object Interface {
    case class A(id: String, other: Int)    extends Interface
    case class B(id: String)                extends Interface
    case class C(id: String, blah: Boolean) extends Interface
  }

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

  @GQLDirective(Directive("key", Map("name" -> StringValue("name"))))
  case class Character(
    @GQLDirective(Directive("external")) name: String,
    @GQLDirective(Directive("required")) nicknames: List[String],
    origin: Origin,
    role: Option[Role]
  )

  @GQLInputName("CharacterInput")
  case class CharacterInput(
    @GQLDirective(Directive("external")) name: String,
    @GQLDirective(Directive("required")) nicknames: List[String],
    origin: Origin
  )

  object Character {
    implicit val schema: Schema[Any, Character] = Schema.gen[Character]
  }

  val characters = List(
    Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain("Rocinante"))),
    Character("Naomi Nagata", Nil, BELT, Some(Engineer("Rocinante"))),
    Character("Amos Burton", Nil, EARTH, Some(Mechanic("Rocinante"))),
    Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante"))),
    Character("Chrisjen Avasarala", Nil, EARTH, None),
    Character("Josephus Miller", List("Joe"), BELT, None),
    Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, None)
  )

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)
  case class CharacterInArgs(@GQLDirective(Directive("lowercase")) names: List[String])
  case class CharacterObjectArgs(character: CharacterInput)

  @GQLDescription("Queries")
  case class Query(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => List[Character],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => Option[Character],
    charactersIn: CharacterInArgs => List[Character],
    exists: CharacterObjectArgs => Boolean
  )

  @GQLDescription("Queries")
  case class QueryIO(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => UIO[Option[Character]]
  )

  @GQLDescription("Mutations")
  case class MutationIO(deleteCharacter: CharacterArgs => UIO[Unit])

  @GQLDescription("Subscriptions")
  case class SubscriptionIO(deleteCharacters: ZStream[Any, Nothing, String])

  val resolver = RootResolver(
    Query(
      args => characters.filter(c => args.origin.forall(c.origin == _)),
      args => characters.find(c => c.name == args.name),
      args => characters.filter(c => args.names.contains(c.name)),
      args => characters.contains(args.character)
    )
  )
  val resolverIO = RootResolver(
    QueryIO(
      args => UIO(characters.filter(c => args.origin.forall(c.origin == _))),
      args => UIO(characters.find(c => c.name == args.name))
    )
  )
  val resolverWithMutation = RootResolver(
    resolverIO.queryResolver,
    MutationIO(_ => UIO.unit)
  )
  val resolverWithSubscription = RootResolver(
    resolver.queryResolver,
    MutationIO(_ => UIO.unit),
    SubscriptionIO(ZStream.empty)
  )

  object InvalidSchemas {
    case class DoubleUnderscoreArg(__name: String)
    case class DoubleUnderscoreInputObjectArg(wrong: DoubleUnderscoreArg)
    case class WrongMutationUnderscore(w: DoubleUnderscoreInputObjectArg => UIO[Unit])

    val resolverWrongMutationUnderscore = RootResolver(
      resolverIO.queryResolver,
      WrongMutationUnderscore(_ => UIO.unit)
    )

    sealed trait UnionInput
    object UnionInput {
      case class A(value: String) extends UnionInput
      case class B(value: String) extends UnionInput
    }
    case class UnionArg(union: UnionInput)
    case class UnionInputObjectArg(wrong: UnionArg)
    case class WrongMutationUnion(w: UnionInputObjectArg => UIO[Unit])

    val resolverWrongMutationUnion = RootResolver(
      resolverIO.queryResolver,
      WrongMutationUnion(_ => UIO.unit)
    )

    object Interface {
      @GQLInterface
      sealed trait InterfaceEmpty
      object InterfaceEmpty {
        case class A(x: String) extends InterfaceEmpty
        case class B(y: String) extends InterfaceEmpty
      }

      case class TestEmpty(i: InterfaceEmpty)
      val resolverEmptyInferface = RootResolver(
        TestEmpty(InterfaceEmpty.A("a"))
      )

      @GQLInterface
      sealed trait InterfaceWrongFieldName
      object InterfaceWrongFieldName {
        case class A(__name: String) extends InterfaceWrongFieldName
        case class B(__name: String) extends InterfaceWrongFieldName
      }

      case class TestWrongFieldName(i: InterfaceWrongFieldName)
      val resolverInferfaceWrongFieldName = RootResolver(
        TestWrongFieldName(InterfaceWrongFieldName.A("a"))
      )

      case class WrongArgumentName(__name: String)

      @GQLInterface
      sealed trait InterfaceWrongArgumentName
      object InterfaceWrongArgumentName {
        case class A(x: WrongArgumentName => UIO[Unit]) extends InterfaceWrongArgumentName
        case class B(x: WrongArgumentName => UIO[Unit]) extends InterfaceWrongArgumentName
      }

      case class TestWrongArgumentName(i: InterfaceWrongArgumentName)
      val resolverInterfaceWrongArgumentName = RootResolver(
        TestWrongArgumentName(InterfaceWrongArgumentName.A(_ => UIO.unit))
      )

      @GQLInterface
      sealed trait InterfaceWrongArgumentInputType
      object InterfaceWrongArgumentInputType {
        case class A(x: UnionInputObjectArg => UIO[Unit]) extends InterfaceWrongArgumentInputType
        case class B(x: UnionInputObjectArg => UIO[Unit]) extends InterfaceWrongArgumentInputType
      }

      case class TestWrongArgumentType(i: InterfaceWrongArgumentInputType)
      val resolverInterfaceWrongArgumentInputType = RootResolver(
        TestWrongArgumentType(InterfaceWrongArgumentInputType.A(_ => UIO.unit))
      )

      case class ClashingObjectArgs(a: ClashingObject)
      case class ClashingObject(a: String)
      case class ClashingObjectInput(a: String)
      case class ClashingQuery(test: ClashingObjectArgs => ClashingObjectInput)
      val resolverClashingObjects = RootResolver(
        ClashingQuery(args => ClashingObjectInput(args.a.a))
      )

      object A {
        case class C(a: String)
      }
      object B {
        case class C(a: String)
      }
      case class ClashingNamesQuery(a: A.C, b: B.C)
      val resolverClashingNames = RootResolver(ClashingNamesQuery(A.C(""), B.C("")))
    }

    object Object {
      case class EmptyObject()
      case class TestEmptyObject(o: EmptyObject)
      val resolverEmptyObject = RootResolver(
        TestEmptyObject(EmptyObject())
      )

      case class ObjectWrongFieldName(__name: String)
      case class TestWrongObjectFieldName(o: ObjectWrongFieldName)
      val resolverObjectWrongFieldName = RootResolver(
        TestWrongObjectFieldName(ObjectWrongFieldName("a"))
      )

      case class ObjectWrongArgumentName(x: WrongArgumentName => UIO[Unit])
      case class TestWrongObjectArgumentName(o: ObjectWrongArgumentName)
      val resolverObjectWrongArgumentName = RootResolver(
        TestWrongObjectArgumentName(ObjectWrongArgumentName(_ => UIO.unit))
      )

      case class ObjectWrongArgumentInputType(x: UnionInputObjectArg => UIO[Unit])
      case class TestWrongObjectArgumentInputType(o: ObjectWrongArgumentInputType)
      val resolverObjectWrongArgumentInputType = RootResolver(
        TestWrongObjectArgumentInputType(ObjectWrongArgumentInputType(_ => UIO.unit))
      )
    }
  }
}
