package caliban.schema

import caliban.Macros._
import caliban.schema.Annotations.{ GQLDescription, GQLInterface, GQLName, GQLUnion }
import caliban.{ graphQL, RootResolver }
import zio.test.{ ZIOSpecDefault, _ }
import zio.{ Chunk, ZIO }

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
    },
    test("i992") {
      import i1992._

      assertTrue(
        schema ==
          """schema {
            |  query: Queries
            |}
            |
            |union Parent = Child1 | Child2 | Child3
            |
            |type Child1 {
            |  bool: Boolean!
            |}
            |
            |type Child2 {
            |  i: Int!
            |}
            |
            |type Child3 {
            |  str: String!
            |}
            |
            |type Queries {
            |  p: Parent!
            |}""".stripMargin
      )
    },
    suite("i1993")(
      test("rendering") {
        import i1993._

        val rendered = schema.render
        assertTrue(
          rendered ==
            """schema {
              |  query: Queries
              |}
              |
              |enum Enum1 {
              |  Item111
              |  Item112
              |  Item121
              |}
              |
              |type Queries {
              |  e1: Enum1!
              |  e2: Enum1!
              |  e3: Enum1!
              |}""".stripMargin
        )
      },
      test("execution") {
        import i1993._

        for {
          i   <- schema.interpreter
          res <- i.execute(gqldoc("{ e1 e2 e3 }"))
          data = res.data.toString
        } yield assertTrue(data == """{"e1":"Item111","e2":"Item112","e3":"Item121"}""")
      }
    ),
    test("nested interfaces execute correctly") {
      import NestedInterfaceIssue._
      for {
        i   <- schema.interpreter
        res <- i.execute(gqldoc("{ e1 { b } e2 { b } e3 { b } }"))
        data = res.data.toString
      } yield assertTrue(data == """{"e1":{"b":"b"},"e2":{"b":"b"},"e3":{"b":"b"}}""")
    },
    test("i2076") {
      import i2076._
      val queryFragments =
        gqldoc("""
            query {
              widget {
                __typename
                ...Widgets
              }
            }

            fragment Widgets on Widget {
              ... on WidgetA {
                name
                children {
                  total
                  nodes { name bar }
                }
              }
              ... on WidgetB {
                name
                children {
                  total
                  nodes { name foo }
                }
              }
            }
            """)

      val queryInlined = gqldoc("""
          query {
            widget {
              __typename
              ... on WidgetA {
                name
                children {
                  total
                  nodes { name bar }
                }
              }
              ... on WidgetB {
                name
                children {
                  total
                  nodes { name foo }
                }
              }
            }
          }
          """)
      for {
        i       <- schema.interpreter
        data1   <- i.execute(queryFragments).map(_.data.toString)
        data2   <- i.execute(queryInlined).map(_.data.toString)
        expected =
          """{"widget":{"__typename":"WidgetB","name":"a","children":{"total":1,"nodes":[{"name":"a","foo":"FOO"}]}}}"""
      } yield assertTrue(data1 == expected, data2 == expected)
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

object i1992 {
  sealed trait Parent
  object Parent {
    implicit val schema: Schema[Any, Parent] = Schema.gen

    sealed trait OtherParent extends Parent
    object OtherParent {
      implicit val schema: Schema[Any, OtherParent] = Schema.gen
    }

    case class Child2(i: Int) extends Parent with OtherParent
    object Child2 {
      implicit val schema: Schema[Any, Child2] = Schema.gen
    }

    case class Child1(bool: Boolean) extends Parent with OtherParent
    object Child1 {
      implicit val schema: Schema[Any, Child1] = Schema.gen
    }

    case class Child3(str: String) extends Parent
    object Child3 {
      implicit val schema: Schema[Any, Child3] = Schema.gen
    }
  }

  case class Queries(p: Parent)

  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(Parent.Child2(1))
    caliban.graphQL(RootResolver(queries))
  }.render
}

object i1993 {
  sealed trait Enum1

  object Enum1 {
    implicit val schema: Schema[Any, Enum1] = Schema.gen

    sealed trait Enum11 extends Enum1

    object Enum11 {
      implicit val schema: Schema[Any, Enum11] = Schema.gen

      case object Item111 extends Enum11 {
        implicit val schema: Schema[Any, Item111.type] = Schema.gen
      }

      case object Item112 extends Enum11 {
        implicit val schema: Schema[Any, Item112.type] = Schema.gen
      }
    }

    sealed trait Enum12 extends Enum1

    object Enum12 {
      implicit val schema: Schema[Any, Enum12] = Schema.gen

      case object Item121 extends Enum12 {
        implicit val schema: Schema[Any, Item121.type] = Schema.gen
      }
    }
  }

  case class Queries(e1: Enum1, e2: Enum1, e3: Enum1)

  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(
      Enum1.Enum11.Item111,
      Enum1.Enum11.Item112,
      Enum1.Enum12.Item121
    )
    caliban.graphQL(RootResolver(queries))
  }
}

object NestedInterfaceIssue {
  import Schema.auto._

  @GQLInterface
  sealed trait NestedInterface

  object NestedInterface {

    @GQLInterface
    sealed trait Mid1 extends NestedInterface

    @GQLInterface
    sealed trait Mid2 extends NestedInterface

    case class FooA(b: String)            extends Mid1
    case class FooB(a: String, b: String) extends Mid1 with Mid2
    case class FooC(c: String, b: String) extends Mid2
  }

  case class Query(e1: NestedInterface, e2: NestedInterface.Mid1, e3: NestedInterface.Mid2)

  implicit lazy val fooA: Schema[Any, NestedInterface.FooA] = Schema.gen
  implicit lazy val fooB: Schema[Any, NestedInterface.FooB] = Schema.gen
  implicit lazy val fooC: Schema[Any, NestedInterface.FooC] = Schema.gen
  implicit lazy val mid1: Schema[Any, NestedInterface.Mid1] = Schema.gen
  implicit lazy val mid2: Schema[Any, NestedInterface.Mid2] = Schema.gen
  implicit lazy val nested: Schema[Any, NestedInterface]    = Schema.gen
  implicit lazy val querySchema: Schema[Any, Query]         = Schema.gen

  val schema = {
    val queries = Query(
      NestedInterface.FooA("b"),
      NestedInterface.FooB("a", "b"),
      NestedInterface.FooC("c", "b")
    )
    caliban.graphQL(RootResolver(queries))
  }
}

object i2076 {
  sealed trait Widget
  object Widget  {
    implicit val schema: Schema[Any, Widget] = Schema.gen

    case class Args(limit: Option[Int])
    object Args {
      implicit val schema: Schema[Any, Args]    = Schema.gen
      implicit val argBuilder: ArgBuilder[Args] = ArgBuilder.gen
    }

    @GQLName("WidgetA")
    case class A(
      name: String,
      children: Args => ZIO[Any, Throwable, A.Connection]
    ) extends Widget
    object A    {
      implicit val schema: Schema[Any, A] = Schema.gen

      @GQLName("WidgetAConnection")
      case class Connection(total: Int, nodes: Chunk[Child])
      object Connection {
        implicit val schema: Schema[Any, Connection] = Schema.gen
      }

      @GQLName("WidgetAChild")
      case class Child(name: String, foo: String, bar: String)
      object Child {
        implicit val schema: Schema[Any, Child] = Schema.gen
      }
    }

    @GQLName("WidgetB")
    case class B(
      name: String,
      children: Args => ZIO[Any, Throwable, B.Connection]
    ) extends Widget
    object B    {
      implicit val schema: Schema[Any, B] = Schema.gen

      @GQLName("WidgetBConnection")
      case class Connection(total: Int, nodes: Chunk[Child])
      object Connection {
        implicit val schema: Schema[Any, Connection] = Schema.gen
      }

      @GQLName("WidgetBChild")
      case class Child(name: String, foo: String)
      object Child {
        implicit val schema: Schema[Any, Child] = Schema.gen
      }
    }
  }

  case class Queries(
    widget: Option[Widget]
  )
  object Queries {
    implicit val schema: Schema[Any, Queries] = Schema.gen
  }

  val schema = {
    val queries = Queries(
      Some(Widget.B("a", _ => ZIO.succeed(Widget.B.Connection(1, Chunk(Widget.B.Child("a", "FOO"))))))
    )
    graphQL(RootResolver(queries))
  }
}
