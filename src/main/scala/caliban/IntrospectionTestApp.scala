package caliban

import caliban.GraphQL._
import caliban.Test.Query
import caliban.schema.Types.Type
import caliban.parsing.Parser
import caliban.schema.{ Schema, Types }
import fastparse.Parsed
import zio.console.putStrLn
import zio.{ App, Runtime, UIO, ZIO }

object IntrospectionTestApp extends App {

  implicit val runtime: Runtime[Environment] = this

  case class __Schema(queryType: Type, types: Set[Type])
  case class TypeArgs(name: String)
  case class Introspection(__schema: __Schema, __type: TypeArgs => Type)

  val introspectionQuery =
    """
    {
      __schema {
        queryType {
          name
          description
        }
        types {
          name
          description
        }
      }
      __type(name: "Character") {
        name
        kind
        description
        fields {
          name
          type {
            name
            kind
            ofType {
              name
              kind
              ofType {
                name
                kind
              }
            }
          }
        }
      }
    }
    """

  implicit lazy val typeSchema: Schema[Type] = Schema.gen[Type]

  val schemaType: Type = Schema.gen[Query].toType
  val types: Set[Type] = Types.collectTypes(schemaType)
  val resolver         = Introspection(__Schema(schemaType, types), args => types.find(_.name.contains(args.name)).get)

  val Parsed.Success(introspection, _) = Parser.parseQuery(introspectionQuery)

  val graph: GraphQL[Introspection] = graphQL[Introspection]

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    (for {
      result <- graph.execute(introspection, resolver)
      _      <- putStrLn(result.mkString("\n"))
    } yield ()).foldM(ex => putStrLn(ex.toString).as(1), _ => UIO.succeed(0))

//    """
//    query IntrospectionQuery {
//      __schema {
//        queryType { name }
//        mutationType { name }
//        subscriptionType { name }
//        types {
//          ...FullType
//        }
//        directives {
//          name
//          description
//          locations
//          args {
//            ...InputValue
//          }
//        }
//      }
//    }
//
//    fragment FullType on __Type {
//      kind
//      name
//      description
//      fields(includeDeprecated: true) {
//        name
//        description
//        args {
//          ...InputValue
//        }
//        type {
//          ...TypeRef
//        }
//        isDeprecated
//        deprecationReason
//      }
//      inputFields {
//        ...InputValue
//      }
//      interfaces {
//        ...TypeRef
//      }
//      enumValues(includeDeprecated: true) {
//        name
//        description
//        isDeprecated
//        deprecationReason
//      }
//      possibleTypes {
//        ...TypeRef
//      }
//    }
//
//    fragment InputValue on __InputValue {
//      name
//      description
//      type { ...TypeRef }
//      defaultValue
//    }
//
//    fragment TypeRef on __Type {
//      kind
//      name
//      ofType {
//        kind
//        name
//        ofType {
//          kind
//          name
//          ofType {
//            kind
//            name
//            ofType {
//              kind
//              name
//              ofType {
//                kind
//                name
//                ofType {
//                  kind
//                  name
//                  ofType {
//                    kind
//                    name
//                  }
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//      """

}
