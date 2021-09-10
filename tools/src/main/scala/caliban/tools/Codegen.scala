package caliban.tools

import caliban.tools.implicits.ScalarMappings
import zio.blocking.{ effectBlocking, Blocking }
import zio.{ RIO, Task, ZIO }

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, StandardCopyOption }

object Codegen {

  sealed trait GenType

  object GenType {
    object Schema extends GenType

    object Client extends GenType
  }

  def generate(
    loader: SchemaLoader,
    arguments: Options,
    genType: GenType
  ): RIO[Blocking, List[File]] = {
    val s                  = ".*/[scala|play.*|app][^/]*/(.*)/(.*).scala".r.findFirstMatchIn(arguments.toPath)
    val packageName        = arguments.packageName.orElse(s.map(_.group(1).split("/").mkString(".")))
    val objectName         = arguments.clientName.orElse(s.map(_.group(2))).getOrElse("Client")
    val abstractEffectType = arguments.abstractEffectType.getOrElse(false)
    val effect             = arguments.effect.getOrElse {
      if (abstractEffectType) "F" else "zio.UIO"
    }
    val genView            = arguments.genView.getOrElse(false)
    val scalarMappings     = arguments.scalarMappings
    val splitFiles         = arguments.splitFiles.getOrElse(false)
    val enableFmt          = arguments.enableFmt.getOrElse(true)
    val extensibleEnums    = arguments.extensibleEnums.getOrElse(false)
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
      paths     <- ZIO.foreach(formatted) { case (objectName, objectCode) =>
                     val file = new File(
                       if (splitFiles) s"${arguments.toPath.reverse.dropWhile(_ != '/').reverse}$objectName.scala"
                       else arguments.toPath
                     )

                     atomicWrite(file, objectCode)
                   }
    } yield paths
  }

  def generate(arguments: Options, genType: GenType): RIO[Blocking, List[File]] =
    generate(
      getSchemaLoader(arguments.schemaPath, arguments.headers),
      arguments,
      genType
    )

  private def getSchemaLoader(path: String, schemaPathHeaders: Option[List[Options.Header]]): SchemaLoader =
    if (path.startsWith("http")) SchemaLoader.fromIntrospection(path, schemaPathHeaders)
    else SchemaLoader.fromFile(path)

  private def atomicWrite(file: File, content: String): RIO[Blocking, File] =
    effectBlocking {
      val tmp        = Files.createTempFile(null, null)
      val tmpWritten = Files.writeString(tmp, content, StandardCharsets.UTF_8)

      Files
        .move(
          tmpWritten,
          file.toPath,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
        .toFile
    }

}
