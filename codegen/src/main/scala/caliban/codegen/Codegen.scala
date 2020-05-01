package caliban.codegen

import java.io.{ File, PrintWriter }
import zio.{ RIO, Task, UIO }
import zio.console.{ putStrLn, Console }
import caliban.parsing.adt.Document
import caliban.parsing.Parser

object Codegen {
  def generate(
    arguments: Options,
    writer: (Document, String, Option[String]) => String
  ): RIO[Console, Unit] =
    for {
      _           <- putStrLn(s"Generating code for ${arguments.schemaPath}")
      s           = ".*/scala/(.*)/(.*).scala".r.findFirstMatchIn(arguments.toPath)
      packageName = s.map(_.group(1).split("/").mkString("."))
      objectName  = s.map(_.group(2)).getOrElse("Client")
      schema      <- getSchema(arguments.schemaPath, arguments.headers)
      code        = writer(schema, objectName, packageName)
      formatted   <- Formatter.format(code, arguments.fmtPath)
      _ <- Task(new PrintWriter(new File(arguments.toPath)))
            .bracket(q => UIO(q.close()), pw => Task(pw.println(formatted)))
      _ <- putStrLn(s"Code generation done")
    } yield ()

  private def getSchema(path: String, schemaPathHeaders: Option[List[Options.Header]]): Task[Document] =
    if (path.startsWith("http")) {
      IntrospectionClient.introspect(path, schemaPathHeaders)
    } else {
      Task(scala.io.Source.fromFile(path))
        .bracket(f => UIO(f.close()), f => Task(f.mkString))
        .flatMap(Parser.parseQuery)
    }

}
