package caliban.schema

import caliban.schema.Annotations.{ GQLDescription, GQLInterface }
import zio.Chunk
import zio.test.ZIOSpecDefault
import zio.test._
import caliban.{ graphQL, RootResolver }

object SchemaDerivationIssuesSpec extends ZIOSpecDefault {
  def spec = suite("SchemaDerivationIssuesSpec")(
    test("i1972 & i1973") {
      import i1972_i1973._

      assertTrue(
        schema ==
          """schema {
            |  query: Queries
            |}
            |
            |union ASTParameter = ASTValueBoolean | ASTValueList | ASTVariable
            |
            |union ASTValue = ASTValueBoolean | ASTValueList
            |
            |type ASTValueBoolean {
            |  value: Boolean!
            |}
            |
            |type ASTValueList {
            |  values: [ASTValue!]!
            |}
            |
            |type ASTVariable {
            |  name: String!
            |}
            |
            |type Queries {
            |  param: ASTParameter!
            |}""".stripMargin
      )
    },
    test("i1951") {
      import i1951._

      assertTrue(
        schema ==
          """schema {
            |  query: Queries
            |}
            |
            |interface MyInterface {
            |  second: String!
            |  "a"
            |  first: String!
            |}
            |
            |type Bar implements MyInterface {
            |  "a"
            |  first: String!
            |  "c"
            |  second: String!
            |}
            |
            |type Foo implements MyInterface {
            |  "a"
            |  first: String!
            |  "b"
            |  second: String!
            |}
            |
            |type Queries {
            |  param: MyInterface!
            |}""".stripMargin
      )
    }
  )
}

private object i1972_i1973 {
  sealed trait Parameter
  object Parameter {
    implicit lazy val schema: Schema[Any, Parameter] = Schema.gen[Any, Parameter].rename("ASTParameter")
  }

  case class Variable(name: String) extends Parameter
  object Variable {
    implicit val schema: Schema[Any, Variable] = Schema.gen[Any, Variable].rename("ASTVariable")
  }

  sealed trait Value extends Parameter
  object Value   {
    case class Boolean(value: scala.Boolean) extends Value
    object Boolean {
      implicit val schema: Schema[Any, Boolean] = Schema.gen[Any, Boolean].rename("ASTValueBoolean")
    }

    case class List(values: Chunk[Value]) extends Value
    object List {
      implicit lazy val schema: Schema[Any, List] = Schema.gen[Any, List].rename("ASTValueList")
    }

    implicit lazy val schema: Schema[Any, Value] = Schema.gen[Any, Value].rename("ASTValue")
  }

  case class Queries(
    param: Parameter
  )
  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(param = Variable("temp"))
    graphQL(RootResolver(queries))
  }.render
}

private object i1951 {
  import Schema.auto._

  @GQLInterface
  sealed trait MyInterface
  object MyInterface {
    case class Foo(@GQLDescription("a") first: String, @GQLDescription("b") second: String) extends MyInterface
    case class Bar(@GQLDescription("a") first: String, @GQLDescription("c") second: String) extends MyInterface
  }

  case class Queries(param: MyInterface)

  val schema = {
    val queries = Queries(param = MyInterface.Foo("foo", "bar"))
    graphQL(RootResolver(queries))
  }.render
}
