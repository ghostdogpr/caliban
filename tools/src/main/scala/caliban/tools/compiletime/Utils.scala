package caliban.tools.compiletime

import java.io.File
import java.nio.file.{ FileAlreadyExistsException, Files }

private[caliban] object Utils {

  def packagePath(packageName: String): String                = packageName.replaceAll("\\.", java.io.File.separator)
  def toPathDir(baseDir: String, packageName: String): String = s"$baseDir/src/main/scala/${packagePath(packageName)}"

  // Copied from better-files.
  def createDirectories(path: String): Unit =
    try Files.createDirectories(new File(path).toPath)
    catch {
      case _: FileAlreadyExistsException => ()
    }

  def toScalaCode[A](l: List[A])(asScalaCode: A => String): String =
    if (l.isEmpty) "List.empty" else s"List(${l.map(asScalaCode).mkString(",")})"

  def toScalaCode(l: List[(String, String)]): String =
    toScalaCode[(String, String)](l)({ case (a, b) => s"""("$a", "$b")"""" })

}
