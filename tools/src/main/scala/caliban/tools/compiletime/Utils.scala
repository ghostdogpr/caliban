package caliban.tools.compiletime

private[caliban] object Utils {

  def packagePath(packageName: String): String                = packageName.split('.').mkString(java.io.File.separator)
  def toPathDir(baseDir: String, packageName: String): String = s"$baseDir/src/main/scala/${packagePath(packageName)}"

  def toScalaCode[A](l: List[A])(asScalaCode: A => String): String =
    if (l.isEmpty) "List.empty" else s"List(${l.map(asScalaCode).mkString(",")})"

  def toScalaCode(l: List[(String, String)]): String =
    toScalaCode[(String, String)](l)({ case (a, b) => s"""("$a", "$b")"""" })

}
