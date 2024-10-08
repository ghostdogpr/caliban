package caliban

import caliban.CalibanError.ParsingError
import caliban.TestUtils._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.Parser
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{
  EnumValueDefinition,
  FieldDefinition,
  InputValueDefinition
}
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.{ Definition, Directive }
import caliban.rendering.DocumentRenderer
import caliban.schema.Annotations.GQLOneOfInput
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.{ ArgBuilder, Schema }
import zio.stream.ZStream
import zio.{ IO, ZIO }
import zio.test.Assertion._
import zio.test._

import scala.io.Source

object RenderingSpec extends ZIOSpecDefault {
  def fixDirectives(directives: List[Directive]): List[Directive] =
    directives.map(_.copy(index = 0)).sortBy(_.name)

  def fixFields(fields: List[FieldDefinition]): List[FieldDefinition] =
    fields.map(field => field.copy(args = fixInputValues(field.args), directives = fixDirectives(field.directives)))

  def fixInputValues(ivs: List[InputValueDefinition]): List[InputValueDefinition] =
    ivs.map(iv => iv.copy(directives = fixDirectives(iv.directives)))

  def fixEnumValues(evs: List[EnumValueDefinition]): List[EnumValueDefinition] =
    evs.map(ev => ev.copy(directives = fixDirectives(ev.directives)))

  def fixDefinitions(definitions: List[Definition]): List[Definition] =
    definitions.map {
      case t: TypeSystemDefinition.SchemaDefinition                         =>
        t.copy(directives = fixDirectives(t.directives))
      case t: TypeDefinition.ObjectTypeDefinition                           =>
        t.copy(directives = fixDirectives(t.directives), fields = fixFields(t.fields))
      case t: TypeSystemDefinition.TypeDefinition.InterfaceTypeDefinition   =>
        t.copy(directives = fixDirectives(t.directives), fields = fixFields(t.fields))
      case t: TypeSystemDefinition.TypeDefinition.InputObjectTypeDefinition =>
        t.copy(directives = fixDirectives(t.directives), fields = fixInputValues(t.fields))
      case t: TypeSystemDefinition.TypeDefinition.EnumTypeDefinition        =>
        t.copy(directives = fixDirectives(t.directives), enumValuesDefinition = fixEnumValues(t.enumValuesDefinition))
      case t: TypeSystemDefinition.TypeDefinition.UnionTypeDefinition       =>
        t.copy(directives = fixDirectives(t.directives))
      case t: TypeSystemDefinition.TypeDefinition.ScalarTypeDefinition      =>
        t.copy(directives = fixDirectives(t.directives))
      case t: TypeSystemExtension.SchemaExtension                           =>
        t.copy(directives = fixDirectives(t.directives))
      case other                                                            => other
    }

  def checkApi[R](api: GraphQL[R]): Either[ParsingError, TestResult] = {
    val definitions = fixDefinitions(api.toDocument.definitions.filter {
      case d: Definition.TypeSystemDefinition.TypeDefinition.ScalarTypeDefinition =>
        !DocumentRenderer.isBuiltinScalar(d.name)
      case _                                                                      => true
    })

    for {
      definitionsFromRender <- Parser.parseQuery(api.render).map(doc => fixDefinitions(doc.definitions))
    } yield assert(definitions)(Assertion.hasSameElements(definitionsFromRender))
  }

  override def spec =
    suite("rendering")(
      test("it should render directives") {
        val api = graphQL(
          resolver,
          directives = List(Directives.Repeatable, Directives.Test),
          schemaDirectives = List(SchemaDirectives.Link)
        )
        checkApi(api)
      },
      test("it should render descriptions") {
        import RenderingSpecSchema._
        val api = graphQL(resolverSchema)
        checkApi(api)
      },
      test("it should render empty objects without field list") {
        assertTrue(graphQL(InvalidSchemas.Object.resolverEmpty).render.trim == """schema {
                                                                                 |  query: TestEmptyObject
                                                                                 |}
                                                                                 |
                                                                                 |type EmptyObject
                                                                                 |
                                                                                 |type TestEmptyObject {
                                                                                 |  o: EmptyObject!
                                                                                 |}""".stripMargin.trim)
      },
      test(
        "it should not render a schema definition without schema directives if no queries, mutations, or subscription"
      ) {
        assertTrue(graphQL(InvalidSchemas.resolverEmpty).render.trim == "")
      },
      test(
        "it should render a schema extension with directives only"
      ) {
        val renderedType =
          graphQL(InvalidSchemas.resolverEmpty, schemaDirectives = List(SchemaDirectives.Link)).render.trim
        assertTrue(
          renderedType == """extend schema @link(url: "https://example.com", import: ["@key", {name: "@provides", as: "@self"}])"""
        )
      },
      test("it should render a schema extension with directives and a mutation") {
        val resolver     = RootResolver(
          Option.empty[Unit],
          Some(MutationIO(_ => ZIO.unit)),
          Option.empty[Unit]
        )
        val renderedType = graphQL(resolver, schemaDirectives = List(SchemaDirectives.Link)).render.trim
        assertTrue(renderedType.startsWith("extend schema"))
      },
      test("it should render a schema extension with directives and a subscription") {
        val resolver     = RootResolver(
          Option.empty[Unit],
          Option.empty[Unit],
          Some(SubscriptionIO(ZStream.empty))
        )
        val renderedType = graphQL(resolver, schemaDirectives = List(SchemaDirectives.Link)).render.trim
        assertTrue(renderedType.startsWith("extend schema"))
      },
      test("it should render object arguments in type directives") {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          directives = Some(
            List(
              Directive(
                name = "testdirective",
                arguments = Map(
                  "object" -> InputValue.ObjectValue(
                    Map(
                      "key1" -> Value.StringValue("value1"),
                      "key2" -> Value.StringValue("value2")
                    )
                  )
                )
              )
            )
          )
        )
        val renderedType = DocumentRenderer.typesRenderer.render(List(testType)).trim
        assertTrue(renderedType == "type TestType @testdirective(object: {key1: \"value1\", key2: \"value2\"})")
      },
      test("only introspectable directives are rendered") {
        val all              = List(
          Directive("d0", isIntrospectable = false),
          Directive("d1"),
          Directive("d2", isIntrospectable = false),
          Directive("d3"),
          Directive("d4"),
          Directive("d5", isIntrospectable = false),
          Directive("d6", isIntrospectable = false)
        )
        val filtered         = all.filter(_.isIntrospectable)
        val renderedAll      = DocumentRenderer.directivesRenderer.render(all)
        val renderedFiltered = DocumentRenderer.directivesRenderer.render(filtered)
        assertTrue(renderedAll == renderedFiltered, renderedAll == " @d1 @d3 @d4")
      },
      test(
        "it should escape \", \\, backspace, linefeed, carriage-return and tab inside a normally quoted description string"
      ) {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          description = Some("A \"TestType\" description with \\, \b, \f, \r and \t")
        )
        val renderedType = DocumentRenderer.typesRenderer.render(List(testType)).trim
        assertTrue(renderedType == "\"A \\\"TestType\\\" description with \\\\, \\b, \\f, \\r and \\t\"\ntype TestType")
      },
      test("it should escape \"\"\" inside a triple-quoted description string") {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          description = Some("A multiline \"TestType\" description\ngiven inside \"\"\"-quotes\n")
        )
        val renderedType = DocumentRenderer.typesRenderer.render(List(testType)).trim
        assertTrue(
          renderedType == "\"\"\"\nA multiline \"TestType\" description\ngiven inside \\\"\"\"-quotes\n\n\"\"\"\ntype TestType"
        )
      },
      test("it should render single line descriptions") {
        val api = graphQL(resolver)
        checkApi(api)
      },
      test("it should render multiple line descriptions") {
        val api = graphQL(resolver)
        checkApi(api)
      },
      test("it should render single line descriptions ending in quote") {
        val api = graphQL(resolver)
        checkApi(api)
      },
      test("it should render multi line descriptions ending in quote") {
        val api = graphQL(resolver)
        checkApi(api)
      },
      test("it should render compact") {
        val rendered = DocumentRenderer.renderCompact(graphQL(resolver).toDocument)
        assertTrue(
          rendered == """schema{query:Query} "Description of custom scalar emphasizing proper captain ship names" scalar CaptainShipName@specifiedBy(url:"http://someUrl")@tag union Role@uniondirective=Captain|Engineer|Mechanic|Pilot enum Origin@enumdirective{BELT,EARTH,MARS,MOON@deprecated(reason:"Use: EARTH | MARS | BELT")} input CharacterInput@inputobjdirective{name:String!@external nicknames:[String!]!@required origin:Origin!}interface Human{name:String!@external}type Captain{shipName:CaptainShipName!}type Character implements Human@key(name:"name"){name:String!@external nicknames:[String!]!@required origin:Origin! role:Role}type Engineer{shipName:String!}type Mechanic{shipName:String!}type Narrator implements Human{name:String!}type Pilot{shipName:String!}"Queries" type Query{"Return all characters from a given origin" characters(origin:Origin):[Character!]! character(name:String!):Character@deprecated(reason:"Use `characters`") charactersIn(names:[String!]!@lowercase):[Character!]! exists(character:CharacterInput!):Boolean! human:Human!}"""
        )
      },
      suite("round-trip")(
        test("kitchen sink")(roundTrip("document-tests/kitchen-sink.graphql")),
        test("kitchen sink with query")(roundTrip("document-tests/kitchen-sink-query.graphql")),
        test("compact query")(roundTrip("document-tests/query-compact.graphql", isCompact = true)),
        test("compact kitchen sink")(roundTrip("document-tests/kitchen-sink-compact.graphql", isCompact = true))
      ),
      suite("OneOf input objects") {
        def expected(label: String) =
          s"""schema {
             |  query: Queries
             |}
             |
             |input FooInput @oneOf {
             |  intValue: FooIntInput
             |  otherIntField: FooInt2Input
             |  otherStringField: String
             |  stringValue: String
             |}
             |
             |input FooInt2Input {
             |  intValue: Int!
             |}
             |
             |input FooIntInput {
             |  intValue: Int!
             |}
             |
             |type Queries {
             |  foo($label: FooInput!): String!
             |}""".stripMargin

        List(
          test("as value types") {
            case class Queries(foo: Foo => String)

            implicit val schema: Schema[Any, Queries] = Schema.gen
            val resolver                              = RootResolver(Queries(_.toString))

            assertTrue(graphQL(resolver).render == expected("value"))
          },
          test("wrapped in a case class") {
            case class Queries(foo: Foo.Wrapped => String)

            implicit val schema: Schema[Any, Queries] = Schema.gen
            val resolver                              = RootResolver(Queries(_.toString))

            assertTrue(graphQL(resolver).render == expected("fooInput"))
          }
        )
      }
    )

  private def roundTrip(file: String, isCompact: Boolean = false) =
    ZIO.scoped(for {
      input   <- ZIO.fromAutoCloseable(ZIO.attempt(Source.fromResource(file))).map(_.mkString)
      doc     <- ZIO.fromEither(Parser.parseQuery(input))
      rendered = if (isCompact) DocumentRenderer.renderCompact(doc) else DocumentRenderer.render(doc)
      reparsed = Parser.parseQuery(rendered)
    } yield assertTrue(input == rendered, reparsed.isRight))

  @GQLOneOfInput
  sealed trait Foo

  object Foo {
    case class ArgA(stringValue: String)      extends Foo
    case class ArgB(otherStringField: String) extends Foo
    case class ArgC(intValue: FooInt)         extends Foo
    case class ArgD(otherIntField: FooInt2)   extends Foo

    case class FooInt(intValue: Int)
    case class FooInt2(intValue: Int)

    case class Wrapped(fooInput: Foo)
  }

  implicit val fooIntAb: ArgBuilder[Foo.FooInt]        = ArgBuilder.gen
  implicit val fooInt2Ab: ArgBuilder[Foo.FooInt2]      = ArgBuilder.gen
  implicit val fooAb: ArgBuilder[Foo]                  = ArgBuilder.gen
  implicit val fooIntSchema: Schema[Any, Foo.FooInt]   = Schema.gen
  implicit val fooInt2Schema: Schema[Any, Foo.FooInt2] = Schema.gen
  implicit val fooSchema: Schema[Any, Foo]             = Schema.gen
}
