package caliban

import cats.effect.IO
import io.circe.Json
import org.openjdk.jmh.annotations._
import sangria.execution._
import sangria.marshalling.circe._
import sangria.parser.QueryParser

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContextExecutor, Future }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class FragmentsQueryBenchmark {
  import FragmentsQueryBenchmark._

  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  @Benchmark
  def fragmentsCaliban(): Unit = {
    val io = Caliban.interpreter.execute(fragmentsQuery)
    Caliban.run(io)
    ()
  }

  @Benchmark
  def fragmentsSangria(): Unit = {
    val future: Future[Json] =
      Future
        .fromTry(QueryParser.parse(fragmentsQuery))
        .flatMap(queryAst => Executor.execute(Sangria.schema, queryAst))
    Await.result(future, 1.minute)
    ()
  }

  @Benchmark
  def fragmentsGrackle(): Unit = {
    val io = Grackle.compileAndRun(fragmentsQuery)
    Grackle.run(io)
    ()
  }

  @Benchmark
  def fragmentsGql(): Unit = {
    val io = gql.Compiler[IO].compile(Gql.schema, fragmentsQuery) match {
      case Right(gql.Application.Query(run)) => run
      case _                                 => IO.raiseError(new Exception("Failed to compile"))
    }
    Gql.run(io)
    ()
  }
}

object FragmentsQueryBenchmark {
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
}
