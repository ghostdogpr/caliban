package caliban.introspection

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.GraphQL._
import caliban.GraphQLResponse
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value._
import caliban.introspection.adt.__Introspection
import caliban.wrappers.Wrapper.IntrospectionWrapper
import zio.ZIO
import zio.test._

object IntrospectionSpec extends ZIOSpecDefault {

  val fullIntrospectionQuery = gqldoc("""
        query IntrospectionQuery {
          __schema {
            queryType { name }
            mutationType { name }
            subscriptionType { name }
            types {
              ...FullType
            }
            directives {
              name
              description
              locations
              args {
                ...InputValue
              }
            }
          }
        }

        fragment FullType on __Type {
          kind
          name
          description
          fields(includeDeprecated: true) {
            name
            description
            args {
              ...InputValue
            }
            type {
              ...TypeRef
            }
            isDeprecated
            deprecationReason
          }
          inputFields {
            ...InputValue
          }
          interfaces {
            ...TypeRef
          }
          enumValues(includeDeprecated: true) {
            name
            description
            isDeprecated
            deprecationReason
          }
          possibleTypes {
            ...TypeRef
          }
        }

        fragment InputValue on __InputValue {
          name
          description
          type { ...TypeRef }
          defaultValue
        }

        fragment TypeRef on __Type {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                    ofType {
                      kind
                      name
                      ofType {
                        kind
                        name
                      }
                    }
                  }
                }
              }
            }
          }
        }
          """)

  override def spec =
    suite("IntrospectionSpec")(
      test("fail when introspection is disabled") {
        val interpreter = graphQL(resolverIO).interpreter

        interpreter
          .flatMap(_.execute(fullIntrospectionQuery, enableIntrospection = false))
          .map { response =>
            assertTrue(response == GraphQLResponse(NullValue, List(ValidationError("Introspection is disabled", ""))))
          }

      },
      test("introspect schema") {
        val interpreter = graphQL(resolverIO).interpreter

        interpreter.flatMap(_.execute(fullIntrospectionQuery)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"__schema":{"queryType":{"name":"QueryIO"},"mutationType":null,"subscriptionType":null,"types":[{"kind":"SCALAR","name":"Boolean","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Captain","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"CaptainShipName","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"CaptainShipName","description":"Description of custom scalar emphasizing proper captain ship names","fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Character","description":null,"fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"nicknames","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"origin","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"Origin","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"role","description":null,"args":[],"type":{"kind":"UNION","name":"Role","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Engineer","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Float","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Int","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Mechanic","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"ENUM","name":"Origin","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"BELT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"EARTH","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"MARS","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"MOON","description":null,"isDeprecated":true,"deprecationReason":"Use: EARTH | MARS | BELT"}],"possibleTypes":null},{"kind":"OBJECT","name":"Pilot","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"QueryIO","description":"Queries","fields":[{"name":"characters","description":"Return all characters from a given origin","args":[{"name":"origin","description":null,"type":{"kind":"ENUM","name":"Origin","ofType":null},"defaultValue":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"Character","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"character","description":null,"args":[{"name":"name","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"defaultValue":null}],"type":{"kind":"OBJECT","name":"Character","ofType":null},"isDeprecated":true,"deprecationReason":"Use `characters`"}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"UNION","name":"Role","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":[{"kind":"OBJECT","name":"Captain","ofType":null},{"kind":"OBJECT","name":"Engineer","ofType":null},{"kind":"OBJECT","name":"Mechanic","ofType":null},{"kind":"OBJECT","name":"Pilot","ofType":null}]},{"kind":"SCALAR","name":"String","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null}],"directives":[{"name":"skip","description":"The @skip directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional exclusion during execution as described by the if argument.","locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[{"name":"if","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null}]},{"name":"include","description":"The @include directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional inclusion during execution as described by the if argument.","locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[{"name":"if","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null}]},{"name":"specifiedBy","description":"The @specifiedBy directive is used within the type system definition language to provide a URL for specifying the behavior of custom scalar types. The URL should point to a human-readable specification of the data format, serialization, and coercion rules. It must not appear on built-in scalar types.","locations":["SCALAR"],"args":[{"name":"url","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"defaultValue":null}]}]}}"""
          )
        }
      },
      test("introspect schema with wrapper") {
        val hideWrapper = new IntrospectionWrapper[Any] {
          def wrap[R1 <: Any](
            effect: ZIO[R1, ExecutionError, __Introspection]
          ): ZIO[R1, ExecutionError, __Introspection] =
            effect.map { intro =>
              intro.copy(__schema =
                intro.__schema.copy(
                  types = intro.__schema.types.collect {
                    case ttp if ttp.name.contains("QueryIO") =>
                      // hide all methods except first
                      ttp.copy(fields = ttp.fields.andThen(_.map(fields => fields.headOption.toList)))
                    case other                               => other
                  }
                )
              )
            }
        }

        val interpreter = (graphQL(resolverIO) @@ hideWrapper).interpreter

        interpreter.flatMap(_.execute(fullIntrospectionQuery)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"__schema":{"queryType":{"name":"QueryIO"},"mutationType":null,"subscriptionType":null,"types":[{"kind":"SCALAR","name":"Boolean","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Captain","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"CaptainShipName","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"CaptainShipName","description":"Description of custom scalar emphasizing proper captain ship names","fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Character","description":null,"fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"nicknames","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"origin","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"Origin","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"role","description":null,"args":[],"type":{"kind":"UNION","name":"Role","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Engineer","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Float","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Int","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Mechanic","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"ENUM","name":"Origin","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"BELT","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"EARTH","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"MARS","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"MOON","description":null,"isDeprecated":true,"deprecationReason":"Use: EARTH | MARS | BELT"}],"possibleTypes":null},{"kind":"OBJECT","name":"Pilot","description":null,"fields":[{"name":"shipName","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"QueryIO","description":"Queries","fields":[{"name":"characters","description":"Return all characters from a given origin","args":[{"name":"origin","description":null,"type":{"kind":"ENUM","name":"Origin","ofType":null},"defaultValue":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"Character","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"UNION","name":"Role","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":[{"kind":"OBJECT","name":"Captain","ofType":null},{"kind":"OBJECT","name":"Engineer","ofType":null},{"kind":"OBJECT","name":"Mechanic","ofType":null},{"kind":"OBJECT","name":"Pilot","ofType":null}]},{"kind":"SCALAR","name":"String","description":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null}],"directives":[{"name":"skip","description":"The @skip directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional exclusion during execution as described by the if argument.","locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[{"name":"if","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null}]},{"name":"include","description":"The @include directive may be provided for fields, fragment spreads, and inline fragments, and allows for conditional inclusion during execution as described by the if argument.","locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[{"name":"if","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null}]},{"name":"specifiedBy","description":"The @specifiedBy directive is used within the type system definition language to provide a URL for specifying the behavior of custom scalar types. The URL should point to a human-readable specification of the data format, serialization, and coercion rules. It must not appear on built-in scalar types.","locations":["SCALAR"],"args":[{"name":"url","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"defaultValue":null}]}]}}"""
          )
        }
      },
      test("introspect type") {
        val interpreter = graphQL(resolverIO).interpreter
        val query       = gqldoc("""
              query {
                __type(name: "Captain") {
                  name
                }
              }
            """)

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"__type":{"name":"Captain"}}""")
        }
      },
      test("introspect non-existent type") {
        val interpreter = graphQL(resolverIO).interpreter
        val query       = gqldoc("""
              query {
                __type(name: "__NonExistent") {
                  name
                }
              }
            """)

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"__type":null}""")
        }
      }
    )
}
