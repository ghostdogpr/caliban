package caliban.schema

import caliban.schema.Annotations.{ GQLInterface, GQLUnion }
import zio.Chunk
import zio.test.ZIOSpecDefault
import zio.test._
import caliban.{ graphQL, RootResolver }

object SchemaDerivationIssuesSpec extends ZIOSpecDefault {
  def spec = suite("SchemaDerivationIssuesSpec")(
    test("i1972 & i1973") {
      import i1972_i1973._

      val schema = {
        val queries = Queries(param = Variable("temp"))
        graphQL(RootResolver(queries))
      }.render

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
    test("i1977") {
      import i1977._

      assertTrue(
        schema ==
          """schema {
            |  query: Queries
            |}
            |
            |union ASTValue = ASTValueBoolean | ASTValueList
            |
            |interface ASTParameter
            |
            |type ASTValueBoolean implements ASTParameter {
            |  value: Boolean!
            |}
            |
            |type ASTValueList implements ASTParameter {
            |  values: [ASTValue!]!
            |}
            |
            |type ASTVariable implements ASTParameter {
            |  name: String!
            |}
            |
            |type Queries {
            |  param: ASTParameter!
            |}""".stripMargin
      )
    }
  )
}

private object i1972_i1973 {
  @GQLUnion
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
  }
}

private object i1977 {
  @GQLInterface
  sealed trait Parameter
  object Parameter {
    implicit lazy val schema: Schema[Any, Parameter] = Schema.gen[Any, Parameter].rename("ASTParameter")
  }

  case class Variable(name: String) extends Parameter
  object Variable {
    implicit val schema: Schema[Any, Variable] = Schema.gen[Any, Variable].rename("ASTVariable")
  }

  sealed trait Value extends Parameter
  object Value {
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

  case class Queries(param: Parameter)
  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(param = Variable("temp"))
    graphQL(RootResolver(queries))
  }.render
}
