package caliban

import caliban.CalibanError.ParsingError
import caliban.TestUtils._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.Parser
import caliban.parsing.adt.Definition.TypeSystemDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{
  EnumValueDefinition,
  FieldDefinition,
  InputValueDefinition
}
import caliban.parsing.adt.{ Definition, Directive }
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import zio.IO
import zio.test.Assertion._
import zio.test._

object RenderingSpec extends ZIOSpecDefault {
  def fixDirectives(directives: List[Directive]): List[Directive] =
    directives.map(_.copy(index = 0))

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
      case other                                                            => other
    }

  def checkApi[R](api: GraphQL[R]): IO[ParsingError, TestResult] = {
    val definitions = fixDefinitions(api.toDocument.definitions.filter {
      case d: Definition.TypeSystemDefinition.TypeDefinition.ScalarTypeDefinition =>
        !Rendering.isBuiltinScalar(d.name)
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
          directives = List(Directives.Test, Directives.Repeatable),
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
      test("it should not render a schema in no queries, mutations, or subscription") {
        assert(graphQL(InvalidSchemas.resolverEmpty).render.trim)(
          equalTo("")
        )
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
        val renderedType = Rendering.renderTypes(List(testType))
        assert(renderedType)(equalTo("type TestType @testdirective(object: {key1: \"value1\",key2: \"value2\"})"))
      },
      test(
        "it should escape \", \\, backspace, linefeed, carriage-return and tab inside a normally quoted description string"
      ) {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          description = Some("A \"TestType\" description with \\, \b, \f, \r and \t")
        )
        val renderedType = Rendering.renderTypes(List(testType))
        assert(renderedType)(
          equalTo("\"A \\\"TestType\\\" description with \\\\, \\b, \\f, \\r and \\t\"\ntype TestType")
        )
      },
      test("it should escape \"\"\" inside a triple-quoted description string") {
        val testType     = __Type(
          __TypeKind.OBJECT,
          name = Some("TestType"),
          description = Some("A multiline \"TestType\" description\ngiven inside \"\"\"-quotes\n")
        )
        val renderedType = Rendering.renderTypes(List(testType))
        assert(renderedType)(
          equalTo("\"\"\"\nA multiline \"TestType\" description\ngiven inside \\\"\"\"-quotes\n\n\"\"\"\ntype TestType")
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
      }
    )
}
