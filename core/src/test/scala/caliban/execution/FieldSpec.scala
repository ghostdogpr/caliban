package caliban.execution

import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.parsing.Parser
import caliban.schema.Annotations.GQLInterface
import caliban.schema._
import caliban.validation.Validator
import zio._
import zio.test._

object FieldSpec extends DefaultRunnableSpec {

  sealed trait Union
  @GQLInterface
  sealed trait Interface {
    def id: Field => UIO[String]
    def inner: Inner
  }

  case class A(id: Field => UIO[String], inner: Inner) extends Union with Interface

  case class B(id: Field => UIO[String], inner: Inner, count: Int) extends Union with Interface

  case class C(id: Field => UIO[String], inner: Inner) extends Interface

  case class Inner(num: Int)

  case class Queries(
    union: Union,
    interface: Interface
  )

  private def api(ref: Ref[Chunk[Field]]) = {
    def track(f: Field) = ref.update(_ :+ f)

    graphQL(
      RootResolver(
        Queries(
          A(track(_).as("id-a"), Inner(1)),
          C(track(_).as("id-c"), Inner(3))
        )
      )
    )
  }

  private def execute(query: String) = for {
    ref    <- Ref.make[Chunk[Field]](Chunk.empty)
    i      <- api(ref).interpreter
    _      <- i.execute(query)
    fields <- ref.get
  } yield fields

  private def prepare(query: String) = for {
    ref     <- Ref.make[Chunk[Field]](Chunk.empty)
    schema  <- api(ref).validateRootSchema
    doc     <- Parser.parseQuery(query)
    rootType = RootType(schema.query.opType, mutationType = None, subscriptionType = None)
    req     <- Validator.prepare(doc, rootType, schema, operationName = None, Map.empty, skipValidation = false)
  } yield req

  private val targetsSpec = suite("targets")(
    testM("gets populated with inline fragments") {
      val query = gqldoc("""{
              union { ...on Interface { id }  }
            }""")

      for {
        fields <- execute(query)
        actual  = fields.flatMap(_.targets.getOrElse(Set.empty)).toSet
      } yield assertTrue(actual == Set("Interface"))
    },
    testM("doesn't get populated with mismatching type conditions") {
      val query = gqldoc("""{
              union { ...on B { id }  }
            }""")

      for {
        fields <- execute(query)
        actual  = fields.flatMap(_.targets.getOrElse(Set.empty)).toSet
      } yield assertTrue(actual == Set.empty[String])
    },
    testM("gets populated with named fragment") {
      val query = gqldoc("""
        fragment Frag on A {
          id
        }
        {
          union { ...Frag }
        }""")

      for {
        fields <- execute(query)
        actual  = fields.flatMap(_.targets.getOrElse(Set.empty)).toSet
      } yield assertTrue(actual == Set("A"))
    },
    testM("gets populated with unnamed fragment") {
      val query = gqldoc("""
        {
          union { ... { id } }
        }""")

      for {
        fields <- execute(query)
        actual  = fields.flatMap(_.targets.getOrElse(Set.empty)).toSet
      } yield assertTrue(actual == Set.empty[String])
    }
  )

  private val fieldTypesSpec = suite("field types")(
    testM("fetching from a union with an interface using an inline fragment") {

      val query = gqldoc("""{
        union {
          ... on Interface {
            id
            inner { num }
          }
          ... on B {
            ... on Interface {
              id
            }
            inner { num }
            count
          }
        }
      }""")

      for {
        req <- prepare(query)
      } yield assertTrue(
        FieldTree.from(req.field) == FieldTree.Node(
          "",
          "Queries",
          List(
            FieldTree.Node(
              "union",
              "Union!",
              List(
                FieldTree.Leaf("id", "String!"),
                FieldTree.Node("inner", "Inner!", List(FieldTree.Leaf("num", "Int!"))),
                FieldTree.Leaf("id", "String!"),
                FieldTree.Node("inner", "Inner!", List(FieldTree.Leaf("num", "Int!"))),
                FieldTree.Leaf("count", "Int!")
              )
            )
          )
        )
      )
    },
    testM("fetching from a union with an interface using a named fragment") {

      val query = gqldoc("""
        fragment Frag on Interface {
          id
          inner { num }
        }
        {
          union {
            ...Frag
            ... on B {
              ...Frag
              count
            }
          }
        }
      """)

      for {
        req <- prepare(query)
      } yield assertTrue(
        FieldTree.from(req.field) == FieldTree.Node(
          "",
          "Queries",
          List(
            FieldTree.Node(
              "union",
              "Union!",
              List(
                FieldTree.Leaf("id", "String!"),
                FieldTree.Node("inner", "Inner!", List(FieldTree.Leaf("num", "Int!"))),
                FieldTree.Leaf("id", "String!"),
                FieldTree.Node("inner", "Inner!", List(FieldTree.Leaf("num", "Int!"))),
                FieldTree.Leaf("count", "Int!")
              )
            )
          )
        )
      )
    },
    testM("fetching from an interface using an inline fragment") {

      val query = gqldoc("""{
        interface {
          ... on Interface {
            id
            inner { num }
          }
          ... on B {
            ... on Interface {
              id
            }
            inner { num }
            count
          }
        }
      }""")

      for {
        req <- prepare(query)
      } yield assertTrue(
        FieldTree.from(req.field) == FieldTree.Node(
          "",
          "Queries",
          List(
            FieldTree.Node(
              "interface",
              "Interface!",
              List(
                FieldTree.Leaf("id", "String!"),
                FieldTree.Node("inner", "Inner!", List(FieldTree.Leaf("num", "Int!"))),
                FieldTree.Leaf("id", "String!"),
                FieldTree.Node("inner", "Inner!", List(FieldTree.Leaf("num", "Int!"))),
                FieldTree.Leaf("count", "Int!")
              )
            )
          )
        )
      )
    }
  )

  override val spec = suite("FieldSpec")(targetsSpec, fieldTypesSpec)

  sealed trait FieldTree {
    def name: String
    def fieldType: String
  }
  object FieldTree       {
    case class Node(name: String, fieldType: String, children: List[FieldTree]) extends FieldTree
    case class Leaf(name: String, fieldType: String)                            extends FieldTree

    def from(field: Field): FieldTree = {
      val name      = field.name
      val fieldType = field.fieldType.toType().toString

      field.fields match {
        case Nil => Leaf(name, fieldType)
        case fs  => Node(name, fieldType, fs.map(from(_)))
      }
    }
  }
}
