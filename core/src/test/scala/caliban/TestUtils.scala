package caliban

import caliban.TestUtils.InvalidSchemas.Interface.WrongArgumentName
import caliban.TestUtils.InvalidSchemas.Object.FieldInterface.FieldObject
import caliban.TestUtils.Origin._
import caliban.TestUtils.Role._
import caliban.Value.StringValue
import caliban.introspection.adt.{ __Field, __InputValue, __Type, __TypeKind }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.Schema.scalarSchema
import caliban.schema.{ Schema, Types }
import zio.stream.ZStream
import zio.{ Task, UIO }

object TestUtils {

  @GQLInterface
  sealed trait Interface
  object Interface {
    final case class A(id: String, other: Int)    extends Interface
    final case class B(id: String)                extends Interface
    final case class C(id: String, blah: Boolean) extends Interface
  }

  sealed trait Origin

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
    @GQLDeprecated("Use: EARTH | MARS | BELT")
    case object MOON  extends Origin
  }

  sealed trait Role

  final case class CaptainShipName(value: String)
  object CaptainShipName {
    implicit val captainShipNameSchema: Schema[Any, CaptainShipName] = scalarSchema(
      "CaptainShipName",
      Some("Description of custom scalar emphasizing proper captain ship names"),
      name => StringValue(name.value)
    )
  }

  object Role {
    final case class Captain(shipName: CaptainShipName) extends Role
    final case class Pilot(shipName: String)            extends Role
    final case class Engineer(shipName: String)         extends Role
    final case class Mechanic(shipName: String)         extends Role
  }

  @GQLDirective(Directive("key", Map("name" -> StringValue("name"))))
  final case class Character(
    @GQLDirective(Directive("external")) name: String,
    @GQLDirective(Directive("required")) nicknames: List[String],
    origin: Origin,
    role: Option[Role]
  )

  final case class OrganizationId(self: Long) extends AnyVal
  final case class Event(organizationId: OrganizationId, title: String)

  final case class Painter(name: String, movement: String)
  final case class WrappedPainter(self: Painter) extends AnyVal

  @GQLInputName("CharacterInput")
  final case class CharacterInput(
    @GQLDirective(Directive("external")) name: String,
    @GQLDirective(Directive("required")) nicknames: List[String],
    origin: Origin
  )

  object Character {
    implicit val schema: Schema[Any, Character] = Schema.gen[Character]
  }

  val characters: List[Character] = List(
    Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain(CaptainShipName("Rocinante")))),
    Character("Naomi Nagata", Nil, BELT, Some(Engineer("Rocinante"))),
    Character("Amos Burton", Nil, EARTH, Some(Mechanic("Rocinante"))),
    Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante"))),
    Character("Chrisjen Avasarala", Nil, EARTH, None),
    Character("Josephus Miller", List("Joe"), BELT, None),
    Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, None)
  )

  final case class CharactersArgs(origin: Option[Origin])
  final case class CharacterArgs(name: String)
  final case class CharacterInArgs(@GQLDirective(Directive("lowercase")) names: List[String])
  final case class CharacterObjectArgs(character: CharacterInput)

  @GQLDescription("Queries")
  final case class Query(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => List[Character],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => Option[Character],
    charactersIn: CharacterInArgs => List[Character],
    exists: CharacterObjectArgs => Boolean
  )

  @GQLDescription("Queries")
  final case class QueryIO(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => UIO[Option[Character]]
  )

  @GQLDescription("Mutations")
  final case class MutationIO(deleteCharacter: CharacterArgs => UIO[Unit])

  final case class SubscriptionIO(deleteCharacters: ZStream[Any, Nothing, String])

  implicit val querySchema: Schema[Any, Query]                   = Schema.gen
  implicit val queryIOSchema: Schema[Any, QueryIO]               = Schema.gen
  implicit val mutationIOSchema: Schema[Any, MutationIO]         = Schema.gen
  implicit val subscriptionIOSchema: Schema[Any, SubscriptionIO] = Schema.gen

  val resolver: RootResolver[Query, Unit, Unit]                                         = RootResolver(
    Query(
      args => characters.filter(c => args.origin.forall(c.origin == _)),
      args => characters.find(c => c.name == args.name),
      args => characters.filter(c => args.names.contains(c.name)),
      args => characters.exists(_.name == args.character.name)
    )
  )
  val resolverIO: RootResolver[QueryIO, Unit, Unit]                                     = RootResolver(
    QueryIO(
      args => UIO(characters.filter(c => args.origin.forall(c.origin == _))),
      args => UIO(characters.find(c => c.name == args.name))
    )
  )
  val resolverWithMutation: RootResolver[Option[QueryIO], MutationIO, Unit]             = RootResolver(
    resolverIO.queryResolver,
    MutationIO(_ => UIO.unit)
  )
  val resolverWithSubscription: RootResolver[Option[Query], MutationIO, SubscriptionIO] = RootResolver(
    resolver.queryResolver,
    MutationIO(_ => UIO.unit),
    SubscriptionIO(ZStream.empty)
  )

  object InvalidSchemas {
    final case class DoubleUnderscoreArg(__name: String)
    final case class DoubleUnderscoreInputObjectArg(wrong: DoubleUnderscoreArg)
    final case class WrongMutationUnderscore(w: DoubleUnderscoreInputObjectArg => UIO[Unit])

    val resolverWrongMutationUnderscore: RootResolver[Option[QueryIO], WrongMutationUnderscore, Unit] = RootResolver(
      resolverIO.queryResolver,
      WrongMutationUnderscore(_ => UIO.unit)
    )

    sealed trait UnionInput
    object UnionInput {
      final case class A(value: String) extends UnionInput
      final case class B(value: String) extends UnionInput
    }
    final case class UnionArg(union: UnionInput)
    final case class UnionInputObjectArg(wrong: UnionArg)
    final case class WrongMutationUnion(w: UnionInputObjectArg => UIO[Unit])

    val resolverWrongMutationUnion: RootResolver[Option[QueryIO], WrongMutationUnion, Unit] = RootResolver(
      resolverIO.queryResolver,
      WrongMutationUnion(_ => UIO.unit)
    )

    object Interface {

      @GQLInterface
      sealed trait InterfaceEmpty

      object InterfaceEmpty {

        final case class A(x: String) extends InterfaceEmpty

        final case class B(y: String) extends InterfaceEmpty

      }

      final case class TestEmpty(i: InterfaceEmpty)

      val resolverEmptyInterface: RootResolver[TestEmpty, Unit, Unit] = RootResolver(
        TestEmpty(InterfaceEmpty.A("a"))
      )

      @GQLInterface
      sealed trait InterfaceWrongFieldName

      object InterfaceWrongFieldName {

        final case class A(__name: String) extends InterfaceWrongFieldName

        final case class B(__name: String) extends InterfaceWrongFieldName

      }

      final case class TestWrongFieldName(i: InterfaceWrongFieldName)

      val resolverInterfaceWrongFieldName: RootResolver[TestWrongFieldName, Unit, Unit] = RootResolver(
        TestWrongFieldName(InterfaceWrongFieldName.A("a"))
      )

      final case class WrongArgumentName(__name: String)

      @GQLInterface
      sealed trait InterfaceWrongArgumentName

      object InterfaceWrongArgumentName {

        final case class A(x: WrongArgumentName => UIO[Unit]) extends InterfaceWrongArgumentName

        final case class B(x: WrongArgumentName => UIO[Unit]) extends InterfaceWrongArgumentName

      }

      final case class TestWrongArgumentName(i: InterfaceWrongArgumentName)

      val resolverInterfaceWrongArgumentName: RootResolver[TestWrongArgumentName, Unit, Unit] = RootResolver(
        TestWrongArgumentName(InterfaceWrongArgumentName.A(_ => UIO.unit))
      )

      @GQLInterface
      sealed trait InterfaceWrongArgumentInputType

      object InterfaceWrongArgumentInputType {

        final case class A(x: UnionInputObjectArg => UIO[Unit]) extends InterfaceWrongArgumentInputType

        final case class B(x: UnionInputObjectArg => UIO[Unit]) extends InterfaceWrongArgumentInputType

      }

      final case class TestWrongArgumentType(i: InterfaceWrongArgumentInputType)

      val resolverInterfaceWrongArgumentInputType: RootResolver[TestWrongArgumentType, Unit, Unit] = RootResolver(
        TestWrongArgumentType(InterfaceWrongArgumentInputType.A(_ => UIO.unit))
      )

      final case class ClashingObjectArgs(a: ClashingObject)

      final case class ClashingObject(a: String)

      final case class ClashingObjectInput(a: String)

      final case class ClashingQuery(test: ClashingObjectArgs => ClashingObjectInput)

      val resolverClashingObjects: RootResolver[ClashingQuery, Unit, Unit] = RootResolver(
        ClashingQuery(args => ClashingObjectInput(args.a.a))
      )

      object A {
        final case class C(a: String)
      }
      object B {
        final case class C(a: String)
      }
      final case class ClashingNamesQuery(a: A.C, b: B.C)
      val resolverClashingNames: RootResolver[ClashingNamesQuery, Unit, Unit] = RootResolver(
        ClashingNamesQuery(A.C(""), B.C(""))
      )
    }

    object Object {
      final case class EmptyObject()
      final case class TestEmptyObject(o: EmptyObject)
      val resolverEmpty: RootResolver[TestEmptyObject, Unit, Unit] = RootResolver(
        TestEmptyObject(EmptyObject())
      )

      final case class ObjectWrongFieldName(__name: String)
      final case class TestWrongObjectFieldName(o: ObjectWrongFieldName)
      val resolverWrongFieldName: RootResolver[TestWrongObjectFieldName, Unit, Unit] = RootResolver(
        TestWrongObjectFieldName(ObjectWrongFieldName("a"))
      )

      final case class ObjectWrongArgumentName(x: WrongArgumentName => UIO[Unit])
      final case class TestWrongObjectArgumentName(o: ObjectWrongArgumentName)
      val resolverWrongArgumentName: RootResolver[TestWrongObjectArgumentName, Unit, Unit] = RootResolver(
        TestWrongObjectArgumentName(ObjectWrongArgumentName(_ => UIO.unit))
      )

      final case class ObjectWrongArgumentInputType(x: UnionInputObjectArg => UIO[Unit])
      final case class TestWrongObjectArgumentInputType(o: ObjectWrongArgumentInputType)
      val resolverWrongArgumentInputType: RootResolver[TestWrongObjectArgumentInputType, Unit, Unit] = RootResolver(
        TestWrongObjectArgumentInputType(ObjectWrongArgumentInputType(_ => UIO.unit))
      )

      @GQLInterface sealed trait InterfaceA {
        def a: Int
      }
      @GQLInterface sealed trait InterfaceB {
        def b: Int
      }
      final case class TwoInterfaceObject(a: Int, b: Int) extends InterfaceA with InterfaceB
      final case class TestTwoInterfaceObject(o: TwoInterfaceObject)
      val resolverTwoInterfaces: RootResolver[TestTwoInterfaceObject, Unit, Unit] = RootResolver(
        TestTwoInterfaceObject(TwoInterfaceObject(0, 1))
      )

      def makeFields(fieldNames: String*): List[__Field] =
        fieldNames.toList
          .map(name =>
            __Field(
              name,
              description = None,
              args = List.empty,
              `type` = () => Types.string
            )
          )

      val interfaceA: __Type = Types.makeInterface(Some("InterfaceA"), None, () => makeFields("a"), Nil)
      val interfaceB: __Type = Types.makeInterface(Some("InterfaceB"), None, () => makeFields("b"), Nil)

      val objectWrongInterfaceFieldType: __Type = __Type(
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

      def objectWithFields(fields: String*): __Type =
        __Type(
          name = Some(s"Fields${fields.mkString("").toUpperCase}"),
          kind = __TypeKind.OBJECT,
          fields = _ => Some(makeFields(fields: _*)),
          interfaces = () => Some(List(interfaceA, interfaceB))
        )

      sealed trait Union
      final case class UnionSubtypeWithA(a: String) extends Union
      final case class TestUnionSubtype(fieldUnion: UnionSubtypeWithA)
      val resolverUnionSubtype: RootResolver[TestUnionSubtype, Unit, Unit] = RootResolver(
        TestUnionSubtype(UnionSubtypeWithA("a"))
      )

      @GQLInterface
      sealed trait FieldInterface {
        val a: String
      }
      object FieldInterface       {
        final case class FieldObject(a: String, b: Int) extends FieldInterface
      }
      final case class TestFieldObject(fieldInterface: FieldObject)
      val resolverFieldObject: RootResolver[TestFieldObject, Unit, Unit] = RootResolver(
        TestFieldObject(FieldObject("a", 1))
      )

      sealed trait WithListFieldUnion {
        val fieldUnions: List[Union]
      }
      final case class TestListUnionSubtype(listFieldUnion: List[UnionSubtypeWithA])
      val resolverListUnionSubtype: RootResolver[TestListUnionSubtype, Unit, Unit] = RootResolver(
        TestListUnionSubtype(List(UnionSubtypeWithA("a")))
      )

      @GQLInterface
      sealed trait WithListFieldInterface {
        val fieldInterfaces: List[FieldInterface]
      }
      final case class TestListInterfaceSubtype(fieldInterfaces: List[FieldObject]) extends WithListFieldInterface
      val resolverListInterfaceSubtype: RootResolver[TestListInterfaceSubtype, Unit, Unit] = RootResolver(
        TestListInterfaceSubtype(List(FieldObject("a", 1)))
      )

      val fieldInterface: __Type             = Types.makeInterface(
        name = Some("FieldInterface"),
        description = None,
        fields = () => List(__Field("a", None, Nil, () => Types.string)),
        subTypes = Nil
      )
      val fieldObject: __Type                = __Type(
        kind = __TypeKind.OBJECT,
        name = Some("FieldObject"),
        interfaces = () => Some(List(fieldInterface)),
        fields = _ => Some(List(__Field("a", None, Nil, () => Types.string)))
      )
      val withListFieldInterface: __Type     = Types.makeInterface(
        name = Some("WithListFieldInterface"),
        description = None,
        fields = () => List(__Field("a", None, Nil, () => Types.makeList(fieldInterface))),
        subTypes = Nil
      )
      val objectWrongListItemSubtype: __Type = __Type(
        kind = __TypeKind.OBJECT,
        name = Some("ObjectWrongListItemSubtype"),
        fields = _ => Some(List(__Field("a", None, Nil, () => Types.makeList(Types.string)))),
        interfaces = () => Some(List(withListFieldInterface))
      )

      @GQLInterface
      sealed trait WithNullable {}

      final case class IsNullable(field: UIO[Option[String]]) extends WithNullable
      final case class IsNonNullable(field: UIO[String])      extends WithNullable

      final case class TestNonNullableObject(nonNullable: WithNullable)
      val resolverNonNullableSubtype: RootResolver[TestNonNullableObject, Unit, Unit] = RootResolver(
        TestNonNullableObject(IsNonNullable(UIO.succeed("a")))
      )

      final case class FieldArg(arg: String)
      @GQLInterface
      sealed trait WithFieldWithArg {
        val fieldWithArg: FieldArg => String
      }
      final case class FieldWithArgObject1(fieldWithArg: FieldArg => String) extends WithFieldWithArg
      final case class FieldWithArgObject2(fieldWithArg: FieldArg => String) extends WithFieldWithArg
      final case class TestFieldWithArgObject(obj: WithFieldWithArg)
      val resolverFieldWithArg: RootResolver[TestFieldWithArgObject, Unit, Unit] = RootResolver(
        TestFieldWithArgObject(FieldWithArgObject1(_ => "a"))
      )

      val nullableExtraArgsObject: __Type = __Type(
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

      val nonNullableExtraArgsObject: __Type = __Type(
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
                    `type` = () => Types.makeNonNull(Types.string),
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
    final case class TestWrongDirectiveName(
      field: String
    )
    val resolverWrongDirectiveName: RootResolver[TestWrongDirectiveName, Unit, Unit] = RootResolver(
      TestWrongDirectiveName("")
    )

    final case class TestWrongFieldDirectiveName(
      @GQLDirective(Directive("__name"))
      field: String
    )
    val resolverWrongFieldDirectiveName: RootResolver[TestWrongFieldDirectiveName, Unit, Unit] = RootResolver(
      TestWrongFieldDirectiveName("")
    )

    final case class TestWrongArgumentDirectiveName(
      @GQLDirective(Directive("name", Map("__name" -> StringValue(""))))
      field: String
    )
    val resolverWrongArgumentDirectiveName: RootResolver[TestWrongArgumentDirectiveName, Unit, Unit] = RootResolver(
      TestWrongArgumentDirectiveName("")
    )

    final case class WrongDirectiveName(
      @GQLDirective(Directive("__name"))
      inputValue: String
    )
    final case class WrongDirectiveNameArgs(
      i: WrongDirectiveName
    )
    final case class TestWrongInputFieldDirectiveName(
      field: WrongDirectiveNameArgs => UIO[Unit]
    )
    val resolverWrongInputFieldDirectiveName: RootResolver[Option[QueryIO], TestWrongInputFieldDirectiveName, Unit] =
      RootResolver(
        resolverIO.queryResolver,
        TestWrongInputFieldDirectiveName(_ => UIO.unit)
      )

    final case class TestWrongFieldArgDirectiveName(
      field: WrongDirectiveName => UIO[Unit]
    )
    val resolverWrongFieldArgDirectiveName: RootResolver[TestWrongFieldArgDirectiveName, Unit, Unit] = RootResolver(
      TestWrongFieldArgDirectiveName(_ => UIO.unit)
    )

  }
}
