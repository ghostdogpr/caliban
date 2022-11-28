package caliban

import caliban.Data._
import caliban.GraphQL._
import io.circe.Json
import org.openjdk.jmh.annotations._
import sangria.execution._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.parser.QueryParser
import caliban.schema.{ Schema => CSchema }
import sangria.schema._
import zio.{ Runtime, Task, UIO, Unsafe, ZIO }

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContextExecutor, Future }
import scala.language.postfixOps

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class GraphQLBenchmarks {
  import CSchema._

  val simpleQuery: String =
    """{
          characters{
            name
          }
       }""".stripMargin

  val fullIntrospectionQuery = """
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
                """

  val fragmentsQuery = s"""
              query IntrospectionQuery {
                __schema {
                  queryType {
                    name
                    ${"...on __Type { name }" * 100}
                  }
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
                              ${"...on __Type { kind name }" * 1000}
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
                """

  private val runtime = Runtime.default

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)

  case class Query(
    characters: CharactersArgs => UIO[List[Character]],
    character: CharacterArgs => UIO[Option[Character]]
  )

  implicit val characterArgsSchema: CSchema[Any, CharacterArgs] = CSchema.gen
  implicit val originSchema: CSchema[Any, Origin]               = CSchema.gen
  implicit val characterSchema: CSchema[Any, Character]         = CSchema.gen

  implicit val querySchema: CSchema[Any, Query] = CSchema.gen

  val resolver: RootResolver[Query, Unit, Unit] = RootResolver(
    Query(
      args => ZIO.succeed(Data.characters.filter(c => args.origin.forall(c.origin == _))),
      args => ZIO.succeed(Data.characters.find(c => c.name == args.name))
    )
  )

  def run[A](zio: Task[A]): A = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())

  val interpreter: GraphQLInterpreter[Any, CalibanError] = run(graphQL[Any, Query, Unit, Unit](resolver).interpreter)

  @Benchmark
  def simpleCaliban(): Unit = {
    val io = interpreter.execute(simpleQuery)
    run(io)
    ()
  }

  @Benchmark
  def introspectCaliban(): Unit = {
    val io = interpreter.execute(fullIntrospectionQuery)
    run(io)
    ()
  }

  @Benchmark
  def fragmentsCaliban(): Unit = {
    val io = interpreter.execute(fragmentsQuery)
    run(io)
    ()
  }

  implicit val OriginEnum: EnumType[Origin]                  = deriveEnumType[Origin](IncludeValues("EARTH", "MARS", "BELT"))
  implicit val CaptainType: ObjectType[Unit, Role.Captain]   = deriveObjectType[Unit, Role.Captain]()
  implicit val PilotType: ObjectType[Unit, Role.Pilot]       = deriveObjectType[Unit, Role.Pilot]()
  implicit val EngineerType: ObjectType[Unit, Role.Engineer] = deriveObjectType[Unit, Role.Engineer]()
  implicit val MechanicType: ObjectType[Unit, Role.Mechanic] = deriveObjectType[Unit, Role.Mechanic]()
  implicit val RoleType: UnionType[Unit]                     = UnionType(
    "Role",
    types = List(PilotType, EngineerType, MechanicType, CaptainType)
  )
  implicit val CharacterType: ObjectType[Unit, Character]    = ObjectType(
    "Character",
    fields[Unit, Character](
      Field(
        "name",
        StringType,
        resolve = _.value.name
      ),
      Field(
        "nicknames",
        ListType(StringType),
        resolve = _.value.nicknames
      ),
      Field(
        "origin",
        OriginEnum,
        resolve = _.value.origin
      ),
      Field(
        "role",
        OptionType(RoleType),
        resolve = _.value.role
      )
    )
  )

  val OriginArg: Argument[Option[Origin]] = Argument("origin", OptionInputType(OriginEnum))
  val NameArg: Argument[String]           = Argument("name", StringType)

  val QueryType: ObjectType[Unit, Unit] = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "characters",
        ListType(CharacterType),
        arguments = OriginArg :: Nil,
        resolve = args => Future.successful(Data.characters.filter(c => (args arg OriginArg).forall(c.origin == _)))
      ),
      Field(
        "character",
        OptionType(CharacterType),
        arguments = NameArg :: Nil,
        resolve = args => Future.successful(Data.characters.find(c => c.name == (args arg NameArg)))
      )
    )
  )

  val schema: Schema[Unit, Unit] = Schema(QueryType)

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  @Benchmark
  def simpleSangria(): Unit = {
    val future: Future[Json] =
      Future.fromTry(QueryParser.parse(simpleQuery)).flatMap(queryAst => Executor.execute(schema, queryAst))
    Await.result(future, 1 minute)
    ()
  }

  @Benchmark
  def introspectSangria(): Unit = {
    val future: Future[Json] =
      Future.fromTry(QueryParser.parse(fullIntrospectionQuery)).flatMap(queryAst => Executor.execute(schema, queryAst))
    Await.result(future, 1 minute)
    ()
  }

  object SangriaNewValidator {
    import sangria.validation.RuleBasedQueryValidator
    import sangria.validation.rules._

    val allRules =
      new RuleBasedQueryValidator(
        List(
          new ValuesOfCorrectType,
          new ExecutableDefinitions,
          new FieldsOnCorrectType,
          new FragmentsOnCompositeTypes,
          new KnownArgumentNames,
          new KnownDirectives,
          new KnownFragmentNames,
          new KnownTypeNames,
          new LoneAnonymousOperation,
          new NoFragmentCycles,
          new NoUndefinedVariables,
          new NoUnusedFragments,
          new NoUnusedVariables,
          new OverlappingFieldsCanBeMerged,
          new PossibleFragmentSpreads,
          new ProvidedRequiredArguments,
          new ScalarLeafs,
          new UniqueArgumentNames,
          new UniqueDirectivesPerLocation,
          new UniqueFragmentNames,
          new UniqueInputFieldNames,
          new UniqueOperationNames,
          new UniqueVariableNames,
          new VariablesAreInputTypes,
          new VariablesInAllowedPosition,
          new InputDocumentNonConflictingVariableInference,
          new SingleFieldSubscriptions
        )
      )
  }

  @Benchmark
  def fragmentsSangriaOld(): Unit = {
    val future: Future[Json] =
      Future
        .fromTry(QueryParser.parse(fragmentsQuery))
        .flatMap(queryAst => Executor.execute(schema, queryAst))
    Await.result(future, 1 minute)
    ()
  }

  @Benchmark
  def fragmentsSangriaNew(): Unit = {
    val future: Future[Json] =
      Future
        .fromTry(QueryParser.parse(fragmentsQuery))
        .flatMap(queryAst => Executor.execute(schema, queryAst, queryValidator = SangriaNewValidator.allRules))
    Await.result(future, 1 minute)
    ()
  }

}
