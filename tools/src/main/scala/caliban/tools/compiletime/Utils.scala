package caliban.tools.compiletime

import java.io.File
import java.nio.file.{ FileAlreadyExistsException, Files }

private[compiletime] object Utils {

  def packagePath(packageName: String): String = packageName.replaceAll("\\.", java.io.File.separator)

  // Copied from better-files.
  def createDirectories(path: String): Unit =
    try Files.createDirectories(new File(path).toPath)
    catch {
      case _: FileAlreadyExistsException => ()
    }

}
