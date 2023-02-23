// package example.play

// import akka.actor.ActorSystem
// import akka.stream.Materializer
// import example.{ ExampleApi, ExampleService }
// import example.ExampleData.sampleCharacters
// import example.ExampleService.ExampleService
// import caliban.PlayAdapter
// import play.api.Mode
// import play.api.routing._
// import play.api.routing.sird._
// import play.core.server.{ AkkaHttpServer, ServerConfig }
// import zio.{ Runtime, Scope, ZIO, ZIOAppDefault }
// import scala.concurrent.ExecutionContextExecutor

// object ExampleApp extends ZIOAppDefault {
//   import sttp.tapir.json.play._

//   override def run =
//     (for {
//       runtime     <- ZIO.runtime[ExampleService]
//       system      <- ZIO.succeed(ActorSystem()).withFinalizer(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
//       interpreter <- ExampleApi.api.interpreter
//       _           <- ZIO.acquireRelease(
//                        ZIO.attempt(
//                          AkkaHttpServer.fromRouterWithComponents(
//                            ServerConfig(
//                              mode = Mode.Dev,
//                              port = Some(8088),
//                              address = "127.0.0.1"
//                            )
//                          ) { _ =>
//                            implicit val ec: ExecutionContextExecutor = system.dispatcher
//                            implicit val mat: Materializer            = Materializer(system)
//                            implicit val rts: Runtime[ExampleService] = runtime
//                            Router.from {
//                              case req @ POST(p"/api/graphql") => PlayAdapter.makeHttpService(interpreter).apply(req)
//                              case req @ GET(p"/ws/graphql")   => PlayAdapter.makeWebSocketService(interpreter).apply(req)
//                            }.routes
//                          }
//                        )
//                      )(server => ZIO.attempt(server.stop()).ignore)
//       _           <- zio.Console.printLine(
//                        "Server online at http://localhost:8088/\nPress RETURN to stop..."
//                      ) *> zio.Console.readLine
//     } yield ()).provideSomeLayer[Scope](ExampleService.make(sampleCharacters))
// }
