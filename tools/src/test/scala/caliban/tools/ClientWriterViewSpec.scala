package caliban.tools

import caliban.parsing.Parser
import zio.test._
import zio.{ Task, ZIO }

object ClientWriterViewSpec extends SnapshotTest {
  override val testName: String = "ClientWriterViewSpec"

  val gen: String => Task[String] = (schema: String) =>
    ZIO
      .fromEither(Parser.parseQuery(schema))
      .flatMap(doc => Formatter.format(ClientWriter.write(doc, genView = true, scalarMappings = None).head._2, None))

  override def spec =
    suite("ClientWriterViewSpec")(
      snapshotTest("simple object type") {
        gen("""
             type Character {
               name: String!
               age: Int!
               nicknames: [String!]!
             }
            """.stripMargin)
      },
      snapshotTest("nested object type") {
        gen("""
             type Q {
               users: [User!]!
             }

             type Character {
               name: String!
               age: Int!
               nicknames(arg: Int): [String!]!
             }

             type User {
               characters(name: String!): [Character!]!
             }
            """.stripMargin)
      },
      snapshotTest("recursive object type") {
        gen("""
             type Character {
               name: String!
               age: Int!
               friends(filter: String): [Character!]!
             }
            """.stripMargin)
      },
      snapshotTest("generic view for scala.Option[List[scala.Option[A]] types") {
        gen("""
            type ProjectMember {
              id: Int
              name: String
            }

            type ProjectMemberEdge {
              cursor: String!
              node: ProjectMember
            }

            type PageInfo {
              endCursor: String
              hasNextPage: Boolean!
              hasPreviousPage: Boolean!
              startCursor: String
            }

            type ProjectMemberConnection {
              edges: [ProjectMemberEdge]
              nodes: [ProjectMember]
              pageInfo: PageInfo!
            }
            """)
      },
      snapshotTest("generic view for scala keywords") {
        gen("""
          type package {
            name: String
          }

          type match {
            package: package
            version: String
          }
            """)
      },
      snapshotTest("generic view for capital fields") {
        gen("""
          type TypeWithCapitalFields {
            Name: String,
            Value: String
          }
            """)
      },
      snapshotTest("union case") {
        gen("""
          type Character {
              name: String!
              nicknames: [String!]!
              role: Role
          }

          union Role = Captain | Pilot
          type Captain {
              shipName: String!
          }
          type Pilot {
              shipName: String!
          }
            """)
      },
      snapshotTest("type with more than 22 fields / function args / selection args") {
        gen("""
             type Big {
               user1: User!
               user2: User!
               user3: User!
               user4: User!
               user5: User!
               user6: User!
               user7: User!
               user8: User!
               user9: User!
               user10: User!
               user11: User!
               user12: User!
               user13: User!
               user14: User!
               user15: User!
               user16: User!
               user17: User!
               user18: User!
               user19: User!
               user20: User!
               user21: User!
               user22: User!
               user23: User!
             }

             type User {
               character1(name: String!): String!
               character2(name: String!): String!
               character3(name: String!): String!
               character4(name: String!): String!
               character5(name: String!): String!
               character6(name: String!): String!
               character7(name: String!): String!
               character8(name: String!): String!
               character9(name: String!): String!
               character10(name: String!): String!
               character11(name: String!): String!
               character12(name: String!): String!
               character13(name: String!): String!
               character14(name: String!): String!
               character15(name: String!): String!
               character16(name: String!): String!
               character17(name: String!): String!
               character18(name: String!): String!
               character19(name: String!): String!
               character20(name: String!): String!
               character21(name: String!): String!
               character22(name: String!): String!
               character23(name: String!): String!
             }
            """.stripMargin)
      },
      snapshotTest("root schema optional interface") {
        gen("""
          schema {
            query: Queries
            mutation: Mutations
            subscription: Subscriptions
          }

          type Queries {
            node(id: ID!): Node
          }

          type Mutations {
            updateNode(id: ID!, name: String): Node
          }

          type Subscriptions {
            node(id: ID!): Node
          }

          interface Node {
            id: ID!
          }
          type NodeA implements Node {
            id: ID!
            a: String
          }
          type NodeB implements Node {
            id: ID!
            b: Int
          }
          """)
      }
    ) @@ TestAspect.parallelN(8)
}
