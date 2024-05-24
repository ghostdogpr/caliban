package caliban.tools

import caliban._
import caliban.introspection.adt._
import caliban.schema._
import caliban.schema.Schema.auto._
import zio._
import zio.test.Assertion._
import zio.test._
import schema.Annotations._
import caliban.Macros.gqldoc
import caliban.execution.Feature
import caliban.transformers.Transformer

object RemoteSchemaSpec extends ZIOSpecDefault {
  sealed trait EnumType  extends Product with Serializable
  case object EnumValue1 extends EnumType
  case object EnumValue2 extends EnumType

  sealed trait UnionType                extends Product with Serializable
  case class UnionValue1(field: String) extends UnionType

  case class Object(
    field: Int,
    optionalField: Option[Float],
    withDefault: Option[String] = Some("defaultValue"),
    enumField: EnumType,
    unionField: UnionType
  )

  object Resolvers {
    def getObject(arg: String = "defaultValue"): Object =
      Object(
        field = 1,
        optionalField = None,
        enumField = EnumValue1,
        unionField = UnionValue1("value")
      )
  }

  case class Queries(
    getObject: String => Object
  )

  val queries = Queries(
    getObject = Resolvers.getObject
  )

  val api = graphQL(
    RootResolver(
      queries
    )
  )

  def spec = suite("ParserSpec")(
    test("is isomorphic") {
      for {
        introspected <- SchemaLoader.fromCaliban(api).load
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(introspected))
        remoteAPI    <- ZIO.succeed(fromRemoteSchema(remoteSchema))
        sdl           = api.render
        remoteSDL     = remoteAPI.render
      } yield assertTrue(remoteSDL == sdl)
    },
    test("properly resolves interface types") {
      @GQLInterface
      sealed trait Node

      sealed trait Viewer
      case class User(id: String, email: String) extends Node with Viewer
      case class Superuser(id: String)           extends Node with Viewer

      case class Queries(
        whoAmI: Node = User("1", "foo@bar.com")
      )

      val api   = graphQL(RootResolver(Queries()))
      val query = gqldoc("""
             query {
               whoAmI {
                 ...on User {
                   email
                 }
                 ...on Node {
                   id
                 }
               }
              }""")

      for {
        introspected <- SchemaLoader.fromCaliban(api).load
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(introspected))
        remoteAPI    <- ZIO.succeed(fromRemoteSchema(remoteSchema))
        interpreter  <- remoteAPI.interpreter
        res          <- interpreter.check(query)
      } yield assert(res)(isUnit)
    }
  )

  def fromRemoteSchema(s: __Schema): GraphQL[Any] =
    new GraphQL[Any] {
      override protected val schemaBuilder                                 =
        RootSchemaBuilder(
          query = Some(
            Operation[Any](
              s.queryType,
              Step.NullStep
            )
          ),
          mutation = None,
          subscription = None
        )
      override protected val additionalDirectives: List[__Directive]       = List()
      override protected val wrappers: List[caliban.wrappers.Wrapper[Any]] = List()
      override protected val features: Set[Feature]                        = Set.empty
      override protected val transformer: Transformer[Any]                 = Transformer.empty
    }

}
