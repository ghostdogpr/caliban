package caliban.tools

import caliban._
import caliban.introspection.Introspector
import caliban.quick._
import caliban.schema.Annotations._
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
  val api     = graphQL(RootResolver(queries)).withAdditionalDirectives(Introspector.directives)

  def spec = suite("IntrospectionClientSpec")(
    test("is isomorphic") {
      val port = 8091
      for {
        _                 <- api.runServer(port = port, apiPath = "/api/graphql").fork
        _                 <- Clock.ClockLive.sleep(2.seconds)
        introspectedSchema =
          SchemaLoader.fromIntrospectionWith(s"http://localhost:$port/api/graphql", None)(_.supportDeprecatedArgs(true))
        codeSchema         = SchemaLoader.fromCaliban(api)
        res               <- SchemaComparison.compare(introspectedSchema, codeSchema)
      } yield assertTrue(res.isEmpty)
    }
  )

}
