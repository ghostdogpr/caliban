package caliban.tools

import caliban.GraphQL._
import caliban._
import caliban.introspection.adt._
import caliban.schema._
import zio._
import zio.test.Assertion._
import zio.test._

object RemoteSchemaSpec extends DefaultRunnableSpec {
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
    testM("is isomorphic") {
      for {
        introspected <- SchemaLoader.fromCaliban(api).load
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(introspected))
        remoteAPI    <- ZIO.effectTotal(fromRemoteSchema(remoteSchema))
        sdl           = api.render
        remoteSDL     = remoteAPI.render
      } yield assert(remoteSDL)(equalTo(sdl))
    }
  )

  def fromRemoteSchema(s: __Schema): GraphQL[Any] =
    new GraphQL[Any] {
      protected val schemaBuilder                                 =
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
      protected val additionalDirectives: List[__Directive]       = List()
      protected val wrappers: List[caliban.wrappers.Wrapper[Any]] = List()
    }

}
