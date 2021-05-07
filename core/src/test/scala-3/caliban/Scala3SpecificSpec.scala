package caliban

import caliban.GraphQL._
import caliban.schema.Annotations.GQLInterface
import zio.test.Assertion._
import zio.test.TestAspect.exceptDotty
import zio.test._
import zio.test.environment.TestEnvironment

object Scala3SpecificSpec extends DefaultRunnableSpec {

  enum MyEnum {
    case A, B, C
  }

  enum MyADT {
    case A(a: Int)
    case B(b: String)
  }

  @GQLInterface
  enum MyADT2 {
    case A(a: Int)
    case B(a: Int)
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Scala3SpecificSpec")(
      testM("Scala 3 enum") {
        case class Queries(item: MyEnum)
        val api         = graphQL(RootResolver(Queries(MyEnum.A)))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"item":"A"}""")
        )
      },
      testM("Scala 3 union") {
        case class Queries(item: MyADT)
        val api         = graphQL(RootResolver(Queries(MyADT.A(1))))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item {
            |     ... on A {
            |       a
            |     }
            |     ... on B {
            |       b
            |     }
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"item":{"a":1}}""")
        )
      },
      testM("Scala 3 interface") {
        case class Queries(item: MyADT2)
        val api         = graphQL(RootResolver(Queries(MyADT2.A(1))))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item {
            |    a
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"item":{"a":1}}""")
        )
      }
    ) @@ exceptDotty
}
