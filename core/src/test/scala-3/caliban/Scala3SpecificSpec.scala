package caliban

import caliban.GraphQL._
import caliban.schema.Annotations.GQLInterface
import zio._
import zio.test._
import zio.test.Assertion._

object Scala3SpecificSpec extends ZIOSpecDefault {

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

  override def spec =
    suite("Scala3SpecificSpec")(
      test("Scala 3 enum") {
        case class Queries(item: MyEnum)
        val api         = graphQL(RootResolver(Queries(MyEnum.A)))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"item":"A"}""")
        }
      },
      test("Scala 3 union") {
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
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"item":{"a":1}}""")
        }
      },
      test("Scala 3 interface") {
        case class Queries(item: MyADT2)
        val api         = graphQL(RootResolver(Queries(MyADT2.A(1))))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  item {
            |    a
            |  }
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"item":{"a":1}}""")
        }
      },
      test("Derive R without imports") {
        case class Inner(io: RIO[Console, String])
        case class Queries(io: RIO[Clock, Int], inner: Inner)
        val api         = graphQL[Clock with Console, Queries, Unit, Unit](
          RootResolver(Queries(ZIO.succeed(1), Inner(ZIO.succeed("ok"))))
        )
        val interpreter = api.interpreter
        val query       =
          """query {
            |  io
            |  inner {
            |    io
            |  }
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"io":1,"inner":{"io":"ok"}}""")
        }
      }.provide(Clock.live ++ Console.live)
    )
}
