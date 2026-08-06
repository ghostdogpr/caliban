package caliban.execution

import caliban._
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.TestUtils._
import caliban.Value.StringValue
import caliban.schema.Annotations.GQLDefault
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import zio.test.Assertion._
import zio.test._

object FragmentSpec extends ZIOSpecDefault {
  override def spec =
    suite("FragmentSpec")(
      test("fragments") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
                   {
                     amos: character(name: "Amos Burton") {
                       ...info
                     }
                   }

                   fragment info on Character {
                     name
                   }""")

        for {
          int <- interpreter
          res <- int.execute(query)
        } yield assertTrue(res.data.toString == """{"amos":{"name":"Amos Burton"}}""")
      },
      test("fragment on root query") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
          {
            ...amos
          }

          fragment amos on Query {
            amos: character(name: "Amos Burton") {
              name
            }
          }
          """)
        for {
          int <- interpreter
          res <- int.execute(query)
        } yield assertTrue(res.data.toString == """{"amos":{"name":"Amos Burton"}}""")
      },
      test("fragment with inner and outer") {
        implicit lazy val bazSchema: Schema[Any, FragmentSchema.Baz] = Schema.gen

        val interpreter = graphQL(FragmentSchema.resolverFooBar).interpreter
        val query       = gqldoc("""
           query {
             bar {
               ...Outer
             }
           }
           fragment Outer on Bar {
             baz {
               foo {
                id
               }
               ...Inner
             }
           }
           fragment Inner on Baz {
             foo {
               id
            }
           }
           """)
        for {
          int <- interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty)
      },
      test("fragment on union") {
        val query = gqldoc("""
                   {
                     amos: character(name: "Amos Burton") {
                       role {
                         ...roleF
                       }
                     }
                   }

                   fragment roleF on Role {
                     ... on Mechanic {
                       shipName
                     }
                   }""")

        for {
          interpreter <- graphQL(resolver).interpreter
          res         <- interpreter.execute(query)
        } yield assertTrue(res.data.toString == """{"amos":{"role":{"shipName":"Rocinante"}}}""")
      },
      test("inline fragment") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
                     {
                       amos: character(name: "Amos Burton") {
                         name
                         role {
                           ... on Mechanic {
                             shipName
                           }
                         }
                       }
                     }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"amos":{"name":"Amos Burton","role":{"shipName":"Rocinante"}}}""")
        }
      },
      test("inline fragment selection with equal field types") {
        sealed trait Union
        case class A(name: String) extends Union
        case class B(name: String) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name }
            |    ...on B { name }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A("Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty)
      },
      test("inline fragment selection with different field types") {
        sealed trait Union
        case class A(name: String)         extends Union
        case class B(name: Option[String]) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name }
            |    ...on B { name }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A("Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(isSome(anything))
      },
      test("inline fragment selection with same arguments") {
        sealed trait Union
        case class A(name: String => String) extends Union
        case class B(name: String => String) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name(value: "hi") }
            |    ...on B { name(value: "hi") }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A(_ => "Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors)(isEmpty)
      },
      test("inline fragment selection with different arguments") {
        sealed trait Union
        case class A(name: Int => String) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name(value: 1) }
            |    ...on A { name(value: 2) }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A(_ => "Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(
          isSome(anything)
        )
      },
      test("nested argument conflict on plain fields when a fragment is present") {
        case class Nested(x: Int => String)
        case class Query(a: Nested)

        val query =
          """query{
            |  a { x(value: 1) }
            |  a { x(value: 2) }
            |  ... on Query { __typename }
            |}""".stripMargin

        val gql = graphQL(RootResolver(Query(Nested(_ => "y"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(
          res.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
            .exists(_.contains("different arguments"))
        )
      },
      test("field-merge conflict is detected on a mutation operation") {
        case class Nested(x: Int => String)
        case class Query(a: Nested)
        case class Mutation(update: Int => Nested)

        val query =
          """mutation{
            |  a: update(value: 1) { x(value: 1) }
            |  a: update(value: 2) { x(value: 1) }
            |  ... on Mutation { __typename }
            |}""".stripMargin

        val gql = graphQL(RootResolver(Query(Nested(_ => "y")), Mutation(_ => Nested(_ => "y"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(
          res.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
            .exists(_.contains("different arguments"))
        )
      },
      test("nested response shapes conflict below mutually-exclusive fragments") {
        case class Owner(name: String, age: Int)
        sealed trait Pet
        case class Dog(friend: Owner) extends Pet
        case class Cat(friend: Owner) extends Pet
        case class Query(pet: Pet)

        val query =
          """query {
            |  pet {
            |    ... on Dog { p: friend { v: name } }
            |    ... on Cat { p: friend { v: age } }
            |  }
            |}""".stripMargin

        val gql = graphQL(RootResolver(Query(Dog(Owner("name", 42)))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(
          res.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
            .exists(_.contains("conflicting types"))
        )
      },
      test("field-merge conflict is detected between named fragments") {
        case class Dog(name: String, nickname: String)
        case class Query(dog: Dog)

        val query =
          """fragment Name on Dog {
            |  value: name
            |}
            |fragment Nickname on Dog {
            |  value: nickname
            |}
            |query {
            |  dog {
            |    ...Name
            |    ...Nickname
            |  }
            |}""".stripMargin

        val gql = graphQL(RootResolver(Query(Dog("name", "nickname"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(
          res.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
            .exists(_.contains("different fields"))
        )
      },
      test("field-merge conflict is detected through nested named fragments") {
        case class Child(value: Int => String)
        case class Query(child: Child)

        val query =
          """fragment One on Child {
            |  ...OneNested
            |}
            |fragment OneNested on Child {
            |  value(value: 1)
            |}
            |fragment Two on Child {
            |  ...TwoNested
            |}
            |fragment TwoNested on Child {
            |  value(value: 2)
            |}
            |query {
            |  child {
            |    ...One
            |    ...Two
            |  }
            |}""".stripMargin

        val gql = graphQL(RootResolver(Query(Child(_.toString))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(
          res.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
            .exists(_.contains("different arguments"))
        )
      },
      test("mutually-exclusive fragment comparison does not hide a later conflict") {
        case class Child(value: Int => String)
        sealed trait Parent
        case class A(child: Child) extends Parent
        case class B(child: Child) extends Parent
        case class Query(parent: Parent, child: Child)

        val mutuallyExclusiveQuery =
          """fragment One on Child {
            |  value(value: 1)
            |}
            |fragment Two on Child {
            |  value(value: 2)
            |}
            |query {
            |  parent {
            |    ... on A { child { ...One } }
            |    ... on B { child { ...Two } }
            |  }
            |}""".stripMargin

        val conflictingQuery =
          """fragment One on Child {
            |  value(value: 1)
            |}
            |fragment Two on Child {
            |  value(value: 2)
            |}
            |query {
            |  parent {
            |    ... on A { child { ...One } }
            |    ... on B { child { ...Two } }
            |  }
            |  child {
            |    ...One
            |    ...Two
            |  }
            |}""".stripMargin

        val child = Child(_.toString)
        val gql   = graphQL(RootResolver(Query(A(child), child)))
        for {
          int         <- gql.interpreter
          valid       <- int.execute(mutuallyExclusiveQuery)
          conflicting <- int.execute(conflictingQuery)
        } yield assertTrue(
          valid.errors.isEmpty,
          conflicting.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
            .exists(_.contains("different arguments"))
        )
      },
      test("nested fragment selection on the same type") {
        import caliban.schema.Schema.auto._

        case class A(valueA: String, valueC: String)
        case class B(valueB: String)
        case class Foo(a: A, b: B)
        case class Query(foo: Foo)

        val api = graphQL(RootResolver(Query(Foo(A("a", "c"), B("b")))))

        val q = gqldoc("""
            fragment FragA on Query {
              foo {
                a {
                  valueA
                  valueC
                }
              }
            }

            fragment FragB on Query {
              foo {
                a {
                  valueA
                }
                b {
                  valueB
                }
              }
            }

            query {
              ...FragA
              ...FragB
            }
            """)

        api.interpreterUnsafe.execute(q).map { res =>
          val d = res.data.toString
          assertTrue(d == """{"foo":{"a":{"valueA":"a","valueC":"c"},"b":{"valueB":"b"}}}""")
        }
      },
      suite("spec examples")(
        suite("simple fields")(
          test("merge identical fields") {
            case class Dog(name: String)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog("Name"))))

            val query =
              """
                |fragment mergeIdenticalFields on Dog {
                |   name
                |   name
                |}
                |query{
                |  dog {
                |    ...mergeIdenticalFields
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assert(res.errors)(isEmpty)
          },
          test("merge identical fields with alias") {
            case class Dog(name: String)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog("Name"))))

            val query =
              """
                |fragment mergeIdenticalFields on Dog {
                |   otherName: name
                |   otherName: name
                |}
                |query{
                |  dog {
                |    ...mergeIdenticalFields
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assert(res.errors)(isEmpty)
          },
          test("alias conflict") {
            case class Dog(name: String, nickname: String)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog("Name", "Nickname"))))

            val query =
              """
                |fragment mergeIdenticalFields on Dog {
                |   name: nickname
                |   name
                |}
                |query{
                |  dog {
                |    ...mergeIdenticalFields
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assert(res.errors.headOption)(isSome(anything))
          }
        ),
        suite("args")(
          test("identical fields with args") {
            sealed trait DogCommand
            case object SIT extends DogCommand
            case class Dog(doesKnowCommand: DogCommand => Boolean)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment mergeIdenticalFieldsWithIdenticalArgs on Dog {
                |   doesKnowCommand(value: SIT)
                |   doesKnowCommand(value: SIT)
                |}
                |query{
                |  dog {
                |    ...mergeIdenticalFieldsWithIdenticalArgs
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assertTrue(res.errors.isEmpty)
          },
          test("identical fields with identical values") {
            sealed trait DogCommand
            case object SIT extends DogCommand
            case class Dog(doesKnowCommand: DogCommand => Boolean)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment mergeIdenticalFieldsWithIdenticalValues on Dog {
                |   doesKnowCommand(value: $dogCommand)
                |   doesKnowCommand(value: $dogCommand)
                |}
                |query DogQuery($dogCommand: DogCommand!){
                |  dog {
                |    ...mergeIdenticalFieldsWithIdenticalValues
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query, variables = Map("dogCommand" -> StringValue("SIT")))
            } yield assertTrue(res.errors.isEmpty)
          },
          test("identical fields with args") {
            sealed trait DogCommand
            case object SIT  extends DogCommand
            case object HEEL extends DogCommand
            case class Dog(doesKnowCommand: DogCommand => Boolean)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment conflictingArgsOnValues on Dog {
                |   doesKnowCommand(value: SIT)
                |   doesKnowCommand(value: HEEL)
                |}
                |query{
                |  dog {
                |    ...conflictingArgsOnValues
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assert(res.errors.headOption)(isSome(anything))
          },
          test("conflicting value and arg") {
            sealed trait DogCommand
            case object SIT extends DogCommand
            case class Dog(doesKnowCommand: DogCommand => Boolean)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment conflictingArgsValueAndVar on Dog {
                |   doesKnowCommand(value: SIT)
                |   doesKnowCommand(value: $dogCommand)
                |}
                |query DogQuery($dogCommand: DogCommand){
                |  dog {
                |    ...conflictingArgsValueAndVar
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query, variables = Map("dogCommand" -> StringValue("SIT")))
            } yield assert(res.errors.headOption)(isSome(anything))
          },
          test("conflicting args") {
            sealed trait DogCommand
            case object SIT extends DogCommand
            case class Dog(doesKnowCommand: DogCommand => Boolean)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment conflictingArgsWithVars on Dog {
                |   doesKnowCommand(value: $varOne)
                |   doesKnowCommand(value: $varTwo)
                |}
                |query DogQuery($varOne: DogCommand, $varTwo: DogCommand){
                |  dog {
                |    ...conflictingArgsWithVars
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query, variables = Map("dogCommand" -> StringValue("SIT")))
            } yield assert(res.errors.headOption)(isSome(anything))
          },
          test("conflicting args") {
            sealed trait DogCommand
            case object SIT extends DogCommand
            case class Dog(@GQLDefault("SIT") doesKnowCommand: DogCommand => Boolean)

            case class Query(dog: Dog)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment differingArgs on Dog {
                |   doesKnowCommand(value: $dogCommand)
                |   doesKnowCommand
                |}
                |query DogQuery($dogCommand: DogCommand){
                |  dog {
                |    ...differingArgs
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query, variables = Map("dogCommand" -> StringValue("SIT")))
            } yield assert(res.errors.headOption)(isSome(anything))
          }
        ),
        suite("different types")(
          test("safe differing fields") {
            sealed trait Pet
            case class Dog(barkVolume: Int) extends Pet
            case class Cat(meowVolume: Int) extends Pet

            case class Query(pet: Pet)
            val gql = graphQL(RootResolver(Query(Dog(1))))

            val query =
              """
                |fragment safeDifferingFields on Pet {
                |  ...on Dog {
                |     volume: barkVolume
                |   }
                |   ...on Cat {
                |     volume: meowVolume
                |   }
                |}
                |query {
                |  pet {
                |    ...safeDifferingFields
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query, variables = Map("dogCommand" -> StringValue("SIT")))
            } yield assertTrue(res.errors.isEmpty)
          },
          test("safe differing args") {
            sealed trait Pet

            sealed trait DogCommand
            case object SIT                                        extends DogCommand
            case class Dog(doesKnowCommand: DogCommand => Boolean) extends Pet

            sealed trait CatCommand
            case object JUMP                                       extends CatCommand
            case class Cat(doesKnowCommand: CatCommand => Boolean) extends Pet

            case class Query(pet: Pet)
            val gql = graphQL(RootResolver(Query(Dog(_ => true))))

            val query =
              """
                |fragment safeDifferingArgs on Pet {
                |  ...on Dog {
                |     doesKnowCommand(value: SIT)
                |   }
                |   ...on Cat {
                |     doesKnowCommand(value: JUMP)
                |   }
                |}
                |query {
                |  pet {
                |    ...safeDifferingArgs
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assertTrue(res.errors.isEmpty)
          },
          test("conflicting different responses") {
            sealed trait Pet

            case class Dog(nickname: String) extends Pet

            case class Cat(meowVolume: Int) extends Pet

            case class Query(pet: Pet)
            val gql = graphQL(RootResolver(Query(Dog("Nickname"))))

            val query =
              """
                |fragment conflictingDifferingResponses on Pet {
                |  ...on Dog {
                |     someValue: nickname
                |   }
                |   ...on Cat {
                |     someValue: meowVolume
                |   }
                |}
                |query {
                |  pet {
                |    ...conflictingDifferingResponses
                |  }
                |}""".stripMargin

            for {
              interpreter <- gql.interpreter
              res         <- interpreter.execute(query)
            } yield assertTrue(
              res.errors.collectFirst { case e: CalibanError.ValidationError => e.msg }
                .exists(_.contains("conflicting types"))
            )
          }
        )
      )
    )
}
