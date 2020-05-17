//package caliban.uzhttp
//
//import java.net.InetSocketAddress
//import _root_.uzhttp.server._
//import caliban.ExampleData._
//import caliban._
//import zio.console.putStrLn
//import zio.{ App, ZEnv, ZIO }
//
//object ExampleApp extends App {
//
//  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
//    (for {
//      interpreter <- ExampleApi.api.interpreter
//      address     = new InetSocketAddress(8088)
//      route       = UzHttpAdapter.makeHttpService("/api/graphql", interpreter)
//      wsRoute     = UzHttpAdapter.makeWebSocketService("/ws/graphql", interpreter)
//      server      = Server.builder(address).handleSome(route orElse wsRoute)
//      _           <- server.serve.useForever.provideCustomLayer(ExampleService.make(sampleCharacters))
//    } yield 0).catchAll(err => putStrLn(err.toString).as(1))
//
//}
