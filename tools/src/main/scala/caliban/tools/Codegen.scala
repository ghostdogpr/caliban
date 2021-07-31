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
  ): Task[List[File]] = {
    val s                  = ".*/[scala|play.*|app][^/]*/(.*)/(.*).scala".r.findFirstMatchIn(arguments.toPath)
    val packageName        = arguments.packageName.orElse(s.map(_.group(1).split("/").mkString(".")))
    val objectName         = s.map(_.group(2)).getOrElse("Client")
    val abstractEffectType = arguments.abstractEffectType.getOrElse(false)
    val effect             = arguments.effect.getOrElse {
      if (abstractEffectType) "F" else "zio.UIO"
    }
    val genView            = arguments.genView.getOrElse(false)
    val scalarMappings     = arguments.scalarMappings
    val splitFiles         = arguments.splitFiles.getOrElse(false)
    val enableFmt          = arguments.enableFmt.getOrElse(true)
    val extensibleEnums    = arguments.extensibleEnums.getOrElse(false)
    val loader             = getSchemaLoader(arguments.schemaPath, arguments.headers)
    for {
      schema    <- loader.load
      code       = genType match {
                     case GenType.Schema =>
                       List(
                         objectName -> SchemaWriter.write(schema, packageName, effect, arguments.imports, abstractEffectType)(
                           ScalarMappings(scalarMappings)
                         )
                       )
                     case GenType.Client =>
                       ClientWriter.write(
                         schema,
                         objectName,
                         packageName,
                         genView,
                         arguments.imports,
                         splitFiles,
                         extensibleEnums
                       )(
                         ScalarMappings(scalarMappings)
                       )
                   }
      formatted <- if (enableFmt) Formatter.format(code, arguments.fmtPath) else Task.succeed(code)
      paths     <- Task.collectAll(formatted.map { case (objectName, objectCode) =>
                     val path =
                       if (splitFiles) s"${arguments.toPath.reverse.dropWhile(_ != '/').reverse}$objectName.scala"
                       else arguments.toPath
                     Task(new PrintWriter(new File(path)))
                       .bracket(q => UIO(q.close()), pw => Task(pw.println(objectCode)))
                       .as(new File(path))
                   })
    } yield paths
  }

  private def getSchemaLoader(path: String, schemaPathHeaders: Option[List[Options.Header]]): SchemaLoader =
    if (path.startsWith("http")) SchemaLoader.fromIntrospection(path, schemaPathHeaders)
    else SchemaLoader.fromFile(path)

}
