package caliban.tools

import java.io.{ File, PrintWriter }

import caliban.tools.implicits.ScalarMappings
import zio.{ Task, UIO }

object Codegen {

  sealed trait GenType

  object GenType {
    object Schema extends GenType

    object Client extends GenType
  }

  def generate(
    arguments: Options,
    genType: GenType
  ): Task[Unit] = {
    val s              = ".*/scala/(.*)/(.*).scala".r.findFirstMatchIn(arguments.toPath)
    val packageName    = arguments.packageName.orElse(s.map(_.group(1).split("/").mkString(".")))
    val objectName     = s.map(_.group(2)).getOrElse("Client")
    val effect         = arguments.effect.getOrElse("zio.UIO")
    val genView        = arguments.genView.getOrElse(false)
    val scalarMappings = arguments.scalarMappings
    val loader         = getSchemaLoader(arguments.schemaPath, arguments.headers)
    for {
      schema    <- loader.load
      code       = genType match {
                     case GenType.Schema =>
                       SchemaWriter.write(schema, packageName, effect, arguments.imports)(ScalarMappings(scalarMappings))
                     case GenType.Client =>
                       ClientWriter.write(schema, objectName, packageName, genView, arguments.imports)(
                         ScalarMappings(scalarMappings)
                       )
                   }
      formatted <- Formatter.format(code, arguments.fmtPath)
      _         <- Task(new PrintWriter(new File(arguments.toPath)))
                     .bracket(q => UIO(q.close()), pw => Task(pw.println(formatted)))
    } yield ()
  }

  private def getSchemaLoader(path: String, schemaPathHeaders: Option[List[Options.Header]]): SchemaLoader =
    if (path.startsWith("http")) SchemaLoader.fromIntrospection(path, schemaPathHeaders)
    else SchemaLoader.fromFile(path)

}
