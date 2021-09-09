package caliban.tools.compiletime

import caliban.tools.compiletime.Config.GenerateClientSettings
import zio.prelude.{ Equivalence, _ }

import java.io.File
import java.nio.file.{ FileAlreadyExistsException, Files }

private[caliban] object Utils {

  def packagePath(packageName: String): String                   = packageName.replaceAll("\\.", java.io.File.separator)
  def toPath(baseDir: String, s: GenerateClientSettings): String =
    s"$baseDir/src/main/scala/${packagePath(s.packageName)}/${s.clientName}.scala"

  // Copied from better-files.
  def createDirectories(path: String): Unit =
    try Files.createDirectories(new File(path).toPath)
    catch {
      case _: FileAlreadyExistsException => ()
    }

  object Equivalences {
    private def emptyValueMarker: String = "_"

    object NoSpaceString extends Subtype[String]
    type NoSpaceString = NoSpaceString.Type

    val noSpaceStringEquivalence: Equivalence[NoSpaceString, String] = {
      val marker = "##"

      Equivalence[NoSpaceString, String](
        to = (nps: NoSpaceString) => nps.replaceAll(marker, " "),
        from = (s: String) => NoSpaceString(s.replaceAll(" ", marker))
      )
    }

    private def optionEquivalence[A](fromString: String => A): Equivalence[Option[A], String] =
      Equivalence[Option[A], String](
        to = _.fold(emptyValueMarker)(_.toString),
        from = s => if (s == emptyValueMarker) None else Some(fromString(s))
      )

    val optionStringEquivalence: Equivalence[Option[String], String] = optionEquivalence[String](identity)

    val tupleStringEquivalence: Equivalence[(String, String), String] = {
      val pattern  = "^\\(\\((.+),,(.+)\\)\\)$".r
      val noSpaces = noSpaceStringEquivalence.from
      val noMarker = noSpaceStringEquivalence.to

      Equivalence[(String, String), String](
        to = { case (a, b) => s"((${noSpaces(a.trim)},,${noSpaces(b.trim)}))" },
        from = { case pattern(a, b) => noMarker(NoSpaceString(a)) -> noMarker(NoSpaceString(b)) }
      )
    }
  }

}
