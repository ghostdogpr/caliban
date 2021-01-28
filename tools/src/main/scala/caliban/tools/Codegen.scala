package caliban.tools

import java.io.{ File, PrintWriter }

import caliban.parsing.adt.Document
import zio.{ Task, UIO }

object Codegen {

  def generate(
    arguments: Options,
    writer: (Document, String, Option[String], String) => String
  ): Task[Unit] = {
    val s           = ".*/scala/(.*)/(.*).scala".r.findFirstMatchIn(arguments.toPath)
    val packageName = arguments.packageName.orElse(s.map(_.group(1).split("/").mkString(".")))
    val objectName  = s.map(_.group(2)).getOrElse("Client")
    val effect      = arguments.effect.getOrElse("zio.UIO")
    val loader      = getSchemaLoader(arguments.schemaPath, arguments.headers)
    for {
      schema    <- loader.load
      code      = writer(schema, objectName, packageName, effect)
      formatted <- Formatter.format(code, arguments.fmtPath)
      _ <- Task(new PrintWriter(new File(arguments.toPath)))
            .bracket(q => UIO(q.close()), pw => Task(pw.println(formatted)))
    } yield ()
  }

  private def getSchemaLoader(path: String, schemaPathHeaders: Option[List[Options.Header]]): SchemaLoader =
    if (path.startsWith("http")) SchemaLoader.fromIntrospection(path, schemaPathHeaders)
    else SchemaLoader.fromFile(path)

}
