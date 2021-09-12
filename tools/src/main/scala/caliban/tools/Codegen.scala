package caliban.tools

import caliban.tools.implicits.ScalarMappings
import zio.blocking.{ effectBlocking, Blocking }
import zio.{ RIO, Task, ZIO, ZManaged }

import java.io.File
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file._

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
                     val path = Path.of(
                       if (splitFiles) s"${arguments.toPath.reverse.dropWhile(_ != '/').reverse}$objectName.scala"
                       else arguments.toPath
                     )

                     atomicWrite(path, objectCode)
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

  private def atomicWrite(
    path: Path,
    content: String,
    copyOptions: List[CopyOption] = List(
      StandardCopyOption.ATOMIC_MOVE,
      StandardCopyOption.REPLACE_EXISTING
    )
  ): RIO[Blocking, File] =
    effectBlocking {
      val tmp: Path        = Files.createTempFile(null, null)
      val tmpWritten: Path = Files.writeString(tmp, content, StandardCharsets.UTF_8)

      Files.move(tmpWritten, path, copyOptions: _*).toFile
    }.catchSome {
      // format: off
      case _: AtomicMoveNotSupportedException => atomicWrite(path, content, copyOptions = List(StandardCopyOption.REPLACE_EXISTING))
      // format: on
    } <* fsync(path)

  /**
   * Quoting Corey O'Connor:
   * """
   * val fd = java.nio.channels.FileChannel.open(dir, StandardOpenOption.READ)
   * fd.force(true)
   *
   * Will fsync a dir. With some creative reading, probably guaranteed by spec. Tho I also verified with strace to be sure ;)
   * """
   *
   * See: https://twitter.com/QueueQueueHack/status/1436923500304887809?s=20
   */
  def fsync(dir: Path): RIO[Blocking, Unit] =
    ZManaged
      .make(effectBlocking(FileChannel.open(dir, StandardOpenOption.READ)))(fd => effectBlocking(fd.close()).orDie)
      .use(fd => effectBlocking(fd.force(true)))

}
