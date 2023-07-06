package caliban

import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.TestUtils.InvalidSchemas.Interface.WrongArgumentName
import caliban.TestUtils.InvalidSchemas.Object.FieldInterface.FieldObject
import caliban.TestUtils.Origin._
import caliban.TestUtils.Role._
import caliban.Value.StringValue
import caliban.introspection.adt.{ __Field, __InputValue, __Type, __TypeKind }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.scalarSchema
import caliban.schema.{ Schema, Types }
import zio.{ UIO, ZIO }
import zio.stream.ZStream
import zio.test.{ TestAspect, TestAspectPoly }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation }

object TestUtils {

  @GQLInterface
  sealed trait Interface
  object Interface {
    case class A(id: String, other: Int)    extends Interface
    case class B(id: String)                extends Interface
    case class C(id: String, blah: Boolean) extends Interface
  }

  @GQLDirective(Directive("enumdirective"))
  sealed trait Origin

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
    @GQLDeprecated("Use: EARTH | MARS | BELT")
    case object MOON  extends Origin
  }

  @GQLDirective(Directive("uniondirective"))
  sealed trait Role

  case class CaptainShipName(value: String)

  implicit val captainShipNameSchema: Schema[Any, CaptainShipName] = scalarSchema(
    "CaptainShipName",
    Some("Description of custom scalar emphasizing proper captain ship names"),
    Some("http://someUrl"),
    Some(List(Directive("tag"))),
    name => StringValue(name.value)
  )

  object Role {
    case class Captain(shipName: CaptainShipName) extends Role
    case class Pilot(shipName: String)            extends Role
    case class Engineer(shipName: String)         extends Role
    case class Mechanic(shipName: String)         extends Role
  }

  @GQLInterface
  sealed trait Human

  @GQLDirective(Directive("key", Map("name" -> StringValue("name"))))
  case class Character(
    @GQLDirective(Directive("external")) name: String,
    @GQLDirective(Directive("required")) nicknames: List[String],
    origin: Origin,
    role: Option[Role]
  ) extends Human

  case class Narrator(
    name: String
  ) extends Human

  case class OrganizationId(self: Long) extends AnyVal
  case class Event(organizationId: OrganizationId, title: String)

  case class Painter(name: String, movement: String)
  case class WrappedPainter(self: Painter) extends AnyVal

  @GQLInputName("CharacterInput")
  @GQLDirective(Directive("inputobjdirective"))
  case class CharacterInput(
    @GQLDirective(Directive("external")) name: String,
    @GQLDirective(Directive("required")) nicknames: List[String],
    origin: Origin
  )

  val characters = List(
    Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain(CaptainShipName("Rocinante")))),
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
    exists: CharacterObjectArgs => Boolean,
    human: Human
  )

  @GQLDescription("Queries")
  case class QueryIO(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => UIO[Option[Character]]
  )

  @GQLDescription("Mutations")
  case class MutationIO(deleteCharacter: CharacterArgs => UIO[Unit])

  case class SubscriptionIO(deleteCharacters: ZStream[Any, Nothing, String])

  implicit val characterSchema: Schema[Any, Character]           = genAll
  implicit val querySchema: Schema[Any, Query]                   = genAll
  implicit val queryIOSchema: Schema[Any, QueryIO]               = genAll
  implicit val mutationIOSchema: Schema[Any, MutationIO]         = genAll
  implicit val subscriptionIOSchema: Schema[Any, SubscriptionIO] = genAll

  val resolver                 = RootResolver(
    Query(
      args => characters.filter(c => args.origin.forall(c.origin == _)),
      args => characters.find(c => c.name == args.name),
      args => characters.filter(c => args.names.contains(c.name)),
      args => characters.exists(_.name == args.character.name),
      Narrator("narrator")
    )
  )
  val resolverIO               = RootResolver(
    QueryIO(
      args => ZIO.succeed(characters.filter(c => args.origin.forall(c.origin == _))),
      args => ZIO.succeed(characters.find(c => c.name == args.name))
    )
  )
  val resolverWithMutation     = RootResolver(
    resolverIO.queryResolver,
    MutationIO(_ => ZIO.unit)
  )
  val resolverWithSubscription = RootResolver(
    resolver.queryResolver,
    MutationIO(_ => ZIO.unit),
    SubscriptionIO(ZStream.empty)
  )

  object Directives {
    val Test = __Directive(
      name = "test",
      description = Some("Test directive"),
      locations = Set(__DirectiveLocation.FIELD_DEFINITION),
      args = List(
        __InputValue(
          "foo",
          None,
          () => Types.int,
          None,
          None
        )
      ),
      isRepeatable = false
    )

    val Repeatable = __Directive(
      name = "repeatable",
      description = Some("Repeatable test directive"),
      locations = Set(__DirectiveLocation.FIELD_DEFINITION),
      args = List(
        __InputValue(
          "bar",
          None,
          () => Types.int,
          None,
          None
        )
      ),
      isRepeatable = true
    )
  }

  object SchemaDirectives {
    val Link = Directive(
      name = "link",
      arguments = Map(
        "url"    -> StringValue("https://example.com"),
        "import" -> ListValue(
          List(StringValue("@key"), ObjectValue(Map("name" -> StringValue("@provides"), "as" -> StringValue("@self"))))
        )
      )
    )
  }

  object InvalidSchemas {
    case class DoubleUnderscoreArg(__name: String)
    case class DoubleUnderscoreInputObjectArg(wrong: DoubleUnderscoreArg)
    case class WrongMutationUnderscore(w: DoubleUnderscoreInputObjectArg => UIO[Unit])

    val resolverWrongMutationUnderscore = RootResolver(
      resolverIO.queryResolver,
      WrongMutationUnderscore(_ => ZIO.unit)
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
      WrongMutationUnion(_ => ZIO.unit)
    )

    object Interface {

      @GQLInterface
      sealed trait InterfaceEmpty

      object InterfaceEmpty {

        case class A(x: String) extends InterfaceEmpty

        case class B(y: String) extends InterfaceEmpty

      }

      case class TestEmpty(i: InterfaceEmpty)

      val resolverEmptyInterface = RootResolver(
        TestEmpty(InterfaceEmpty.A("a"))
      )

      @GQLInterface
      sealed trait InterfaceWrongFieldName

      object InterfaceWrongFieldName {

        case class A(__name: String) extends InterfaceWrongFieldName

        case class B(__name: String) extends InterfaceWrongFieldName

      }

      case class TestWrongFieldName(i: InterfaceWrongFieldName)

      val resolverInterfaceWrongFieldName = RootResolver(
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
        TestWrongArgumentName(InterfaceWrongArgumentName.A(_ => ZIO.unit))
      )

      @GQLInterface
      sealed trait InterfaceWrongArgumentInputType

      object InterfaceWrongArgumentInputType {

        case class A(x: UnionInputObjectArg => UIO[Unit]) extends InterfaceWrongArgumentInputType

        case class B(x: UnionInputObjectArg => UIO[Unit]) extends InterfaceWrongArgumentInputType

      }

      case class TestWrongArgumentType(i: InterfaceWrongArgumentInputType)

      val resolverInterfaceWrongArgumentInputType = RootResolver(
        TestWrongArgumentType(InterfaceWrongArgumentInputType.A(_ => ZIO.unit))
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
      val resolverEmpty = RootResolver(
        TestEmptyObject(EmptyObject())
      )

      case class ObjectWrongFieldName(__name: String)
      case class TestWrongObjectFieldName(o: ObjectWrongFieldName)
      val resolverWrongFieldName = RootResolver(
        TestWrongObjectFieldName(ObjectWrongFieldName("a"))
      )

      case class ObjectWrongArgumentName(x: WrongArgumentName => UIO[Unit])
      case class TestWrongObjectArgumentName(o: ObjectWrongArgumentName)
      val resolverWrongArgumentName = RootResolver(
        TestWrongObjectArgumentName(ObjectWrongArgumentName(_ => ZIO.unit))
      )

      case class ObjectWrongArgumentInputType(x: UnionInputObjectArg => UIO[Unit])
      case class TestWrongObjectArgumentInputType(o: ObjectWrongArgumentInputType)
      val resolverWrongArgumentInputType = RootResolver(
        TestWrongObjectArgumentInputType(ObjectWrongArgumentInputType(_ => ZIO.unit))
      )

      @GQLInterface sealed trait InterfaceA {
        def a: Int
      }
      @GQLInterface sealed trait InterfaceB {
        def b: Int
      }
      case class TwoInterfaceObject(a: Int, b: Int) extends InterfaceA with InterfaceB
      case class TestTwoInterfaceObject(o: TwoInterfaceObject)
      val resolverTwoInterfaces = RootResolver(
        TestTwoInterfaceObject(TwoInterfaceObject(0, 1))
      )

      def makeFields(fieldNames: String*) =
        fieldNames.toList
          .map(name =>
            __Field(
              name,
              description = None,
              args = List.empty,
              `type` = () => Types.string
            )
          )

      val interfaceA = Types.makeInterface(Some("InterfaceA"), None, () => makeFields("a"), Nil)
      val interfaceB = Types.makeInterface(Some("InterfaceB"), None, () => makeFields("b"), Nil)

      val objectWrongInterfaceFieldType = __Type(
        name = Some("ObjectWrongInterfaceFieldType"),
        kind = __TypeKind.OBJECT,
        interfaces = () => Some(List(interfaceA, interfaceB)),
        fields = _ =>
          Some(
            List(
              __Field(
                name = "a",
                description = None,
                args = Nil,
                `type` = () => Types.int     // bad type, interface type is string
              ),
              __Field(
                name = "b",
                description = None,
                args = Nil,
                `type` = () => Types.boolean // bad type, interface type is string
              )
            )
          )
      )

      def objectWithFields(fields: String*) =
        __Type(
          name = Some(s"Fields${fields.mkString("").toUpperCase}"),
          kind = __TypeKind.OBJECT,
          fields = _ => Some(makeFields(fields: _*)),
          interfaces = () => Some(List(interfaceA, interfaceB))
        )

      sealed trait Union
      case class UnionSubtypeWithA(a: String) extends Union
      case class TestUnionSubtype(fieldUnion: UnionSubtypeWithA)
      val resolverUnionSubtype = RootResolver(TestUnionSubtype(UnionSubtypeWithA("a")))

      @GQLInterface
      sealed trait FieldInterface {
        val a: String
      }
      object FieldInterface       {
        case class FieldObject(a: String, b: Int) extends FieldInterface
      }
      case class TestFieldObject(fieldInterface: FieldObject)
      val resolverFieldObject = RootResolver(TestFieldObject(FieldObject("a", 1)))

      sealed trait WithListFieldUnion {
        val fieldUnions: List[Union]
      }
      case class TestListUnionSubtype(listFieldUnion: List[UnionSubtypeWithA])
      val resolverListUnionSubtype = RootResolver(TestListUnionSubtype(List(UnionSubtypeWithA("a"))))

      @GQLInterface
      sealed trait WithListFieldInterface {
        val fieldInterfaces: List[FieldInterface]
      }
      case class TestListInterfaceSubtype(fieldInterfaces: List[FieldObject]) extends WithListFieldInterface
      val resolverListInterfaceSubtype = RootResolver(TestListInterfaceSubtype(List(FieldObject("a", 1))))

      val fieldInterface             = Types.makeInterface(
        name = Some("FieldInterface"),
        description = None,
        fields = () => List(__Field("a", None, Nil, () => Types.string)),
        subTypes = Nil
      )
      val fieldObject                = __Type(
        kind = __TypeKind.OBJECT,
        name = Some("FieldObject"),
        interfaces = () => Some(List(fieldInterface)),
        fields = _ => Some(List(__Field("a", None, Nil, () => Types.string)))
      )
      val withListFieldInterface     = Types.makeInterface(
        name = Some("WithListFieldInterface"),
        description = None,
        fields = () => List(__Field("a", None, Nil, () => fieldInterface.list)),
        subTypes = Nil
      )
      val objectWrongListItemSubtype = __Type(
        kind = __TypeKind.OBJECT,
        name = Some("ObjectWrongListItemSubtype"),
        fields = _ => Some(List(__Field("a", None, Nil, () => Types.string.list))),
        interfaces = () => Some(List(withListFieldInterface))
      )

      @GQLInterface
      sealed trait WithNullable {}

      case class IsNullable(field: UIO[Option[String]]) extends WithNullable
      case class IsNonNullable(field: UIO[String])      extends WithNullable

      case class TestNonNullableObject(nonNullable: WithNullable)
      val resolverNonNullableSubtype = RootResolver(TestNonNullableObject(IsNonNullable(ZIO.succeed("a"))))

      case class FieldArg(arg: String)
      @GQLInterface
      sealed trait WithFieldWithArg {
        val fieldWithArg: FieldArg => String
      }
      case class FieldWithArgObject1(fieldWithArg: FieldArg => String) extends WithFieldWithArg
      case class FieldWithArgObject2(fieldWithArg: FieldArg => String) extends WithFieldWithArg
      case class TestFieldWithArgObject(obj: WithFieldWithArg)
      val resolverFieldWithArg = RootResolver(TestFieldWithArgObject(FieldWithArgObject1(_ => "a")))

      val nullableExtraArgsObject = __Type(
        name = Some("NullableExtraArgsObject"),
        kind = __TypeKind.OBJECT,
        description = None,
        fields = _ =>
          Some(
            List(
              __Field(
                "fieldWithArg",
                None,
                List(
                  __InputValue(name = "arg", description = None, `type` = () => Types.string, defaultValue = None),
                  __InputValue(name = "extraArg", description = None, `type` = () => Types.string, defaultValue = None)
                ),
                () => Types.string
              )
            )
          ),
        interfaces = () => Some(List(withNullableExtraArgs)),
        directives = None
      )

      val nonNullableExtraArgsObject = __Type(
        name = Some("NonNullableExtraArgsObject"),
        kind = __TypeKind.OBJECT,
        description = None,
        fields = _ =>
          Some(
            List(
              __Field(
                "fieldWithArg",
                None,
                List(
                  __InputValue(name = "arg", description = None, `type` = () => Types.string, defaultValue = None),
                  __InputValue(
                    name = "extraArg",
                    description = None,
                    `type` = () => Types.string.nonNull,
                    defaultValue = None
                  )
                ),
                () => Types.string
              )
            )
          ),
        interfaces = () => Some(List(withNullableExtraArgs)),
        directives = None
      )

      val withNullableExtraArgs: __Type = Types.makeInterface(
        name = Some("WithNullableExtraArgs"),
        description = None,
        fields = () =>
          List(
            __Field(
              name = "fieldWithArg",
              description = None,
              `type` = () => Types.string,
              args =
                List(__InputValue(name = "arg", description = None, `type` = () => Types.string, defaultValue = None))
            )
          ),
        subTypes = List(nullableExtraArgsObject)
      )
    }

    @GQLDirective(Directive("__name"))
    case class TestWrongDirectiveName(
      field: String
    )
    val resolverWrongDirectiveName = RootResolver(TestWrongDirectiveName(""))

    case class TestWrongFieldDirectiveName(
      @GQLDirective(Directive("__name"))
      field: String
    )
    val resolverWrongFieldDirectiveName = RootResolver(TestWrongFieldDirectiveName(""))

    case class TestWrongArgumentDirectiveName(
      @GQLDirective(Directive("name", Map("__name" -> StringValue(""))))
      field: String
    )
    val resolverWrongArgumentDirectiveName = RootResolver(TestWrongArgumentDirectiveName(""))

    case class WrongDirectiveName(
      @GQLDirective(Directive("__name"))
      inputValue: String
    )
    case class WrongDirectiveNameArgs(
      i: WrongDirectiveName
    )
    case class TestWrongInputFieldDirectiveName(
      field: WrongDirectiveNameArgs => UIO[Unit]
    )
    val resolverWrongInputFieldDirectiveName = RootResolver(
      resolverIO.queryResolver,
      TestWrongInputFieldDirectiveName(_ => ZIO.unit)
    )

    case class TestWrongFieldArgDirectiveName(
      field: WrongDirectiveName => UIO[Unit]
    )
    val resolverWrongFieldArgDirectiveName   = RootResolver(
      TestWrongFieldArgDirectiveName(_ => ZIO.unit)
    )

    val resolverEmpty = new RootResolver(Option.empty[Unit], Option.empty[Unit], Option.empty[Unit])

    @GQLName("Foo { str: String }\ntype Bar")
    case class WrongTypeName(num: Int)
    val resolverBadTypeName = RootResolver(WrongTypeName(1))

    case class WrongFieldName(@GQLName("bool: Boolean\n  num") num: Int)
    val resolverBadFieldName = RootResolver(WrongFieldName(1))

    val resolverQueryNoObject = RootResolver(Map("a" -> "b"))
  }
}
