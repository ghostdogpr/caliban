package caliban.schema

import caliban.schema.Annotations.{ GQLDescription, GQLInterface, GQLName, GQLUnion }
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
            |  "a"
            |  first: String!
            |  second: String!
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
    },
    test("i1989") {
      import i1989._

      assertTrue(
        schema ==
          """schema {
            |  query: Queries
            |}
            |
            |union Level1 = Level3
            |
            |type Level3 {
            |  value: Boolean!
            |}
            |
            |type Queries {
            |  level: Level1!
            |}""".stripMargin
      )
    },
    test("i1990") {
      import i1990._

      assertTrue(
        schema ==
          """schema {
            |  query: Queries
            |}
            |
            |enum PrefixOperator {
            |  PrefixOperatorGreaterThan
            |  PrefixOperatorLessThan
            |}
            |
            |type Queries {
            |  op: PrefixOperator!
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
    val queries = Queries(param = MyInterface.Foo("a", "b"))
    graphQL(RootResolver(queries))
  }.render
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

private object i1989 {
  sealed trait Level1
  object Level1 {
    implicit val schema: Schema[Any, Level1] = Schema.gen

    sealed trait Level2 extends Level1
    object Level2 {
      implicit val schema: Schema[Any, Level2] = Schema.gen

      case class Level3(value: Boolean) extends Level2
      object Level3 {
        implicit val schema: Schema[Any, Level3] = Schema.gen
      }
    }
  }

  case class Queries(level: Level1)
  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(level = Level1.Level2.Level3(false))
    caliban.graphQL(RootResolver(queries))
  }.render
}

object i1990 {
  @GQLName("PrefixOperator")
  sealed trait Operator
  object Operator {
    @GQLName("PrefixOperatorGreaterThan")
    case object GreaterThan extends Operator {
      implicit val schema: Schema[Any, GreaterThan.type] = Schema.gen
    }

    @GQLName("PrefixOperatorLessThan")
    case object LessThan extends Operator {
      implicit val schema: Schema[Any, LessThan.type] = Schema.gen
    }

    implicit val schema: Schema[Any, Operator] = Schema.gen
  }

  case class Queries(op: Operator)

  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(op = Operator.LessThan)
    caliban.graphQL(RootResolver(queries))
  }.render
}
