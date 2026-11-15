package caliban.tools

import caliban._
import caliban.introspection.Introspector
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue }
import caliban.quick._
import caliban.schema.Annotations._
import caliban.schema.Types
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import zio.test._
import zio.{ durationInt, Clock }

object IntrospectionClientSpec extends ZIOSpecDefault {

  case class Args(@GQLDeprecated("Use nameV2") name: Option[String] = Some("defaultValue"), nameV2: String)
  case class Queries(getObject: Args => String)

  object Resolvers {
    def getObject(@GQLDeprecated("foobar") args: Args): String = args.name.getOrElse("")
  }

  val queries = Queries(getObject = Resolvers.getObject)
  val legacy  = __Directive(
    "legacy",
    None,
    Set(__DirectiveLocation.FIELD_DEFINITION),
    _.filter(List(__InputValue("old", None, () => Types.string, None, true, Some("Unused"))))(_.isDeprecated),
    isRepeatable = false,
    isDeprecated = true,
    deprecationReason = Some("Use @deprecated")
  )
  val api     = graphQL(RootResolver(queries)).withAdditionalDirectives(Introspector.directives :+ legacy)

  def spec = suite("IntrospectionClientSpec")(
    test("is isomorphic") {
      val port = 8091
      for {
        _                 <- api.runServer(port = port, apiPath = "/api/graphql").fork
        _                 <- Clock.ClockLive.sleep(2.seconds)
        introspectedSchema =
          SchemaLoader.fromIntrospectionWith(s"http://localhost:$port/api/graphql", None)(
            _.supportDeprecatedArgs(true).supportDeprecatedDirectives(true)
          )
        codeSchema         = SchemaLoader.fromCaliban(api)
        res               <- SchemaComparison.compare(introspectedSchema, codeSchema)
      } yield assertTrue(res.isEmpty)
    }
  )

}
