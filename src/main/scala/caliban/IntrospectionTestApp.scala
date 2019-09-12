package caliban

import caliban.Test.Query
import caliban.Types.Type
import fastparse.Parsed

object IntrospectionTestApp extends App {

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

  implicit lazy val typeExecuter: Executer[Type] = Executer.gen[Type]

  val schemaType = Schema.gen[Query].toType
  val types      = Types.collectTypes(schemaType)
  val resolver   = Introspection(__Schema(schemaType, types), args => types.find(_.name.contains(args.name)).get)

  val Parsed.Success(introspection, _) = Parser.parseQuery(introspectionQuery)
  println(GraphQL.execute(introspection, resolver))

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
