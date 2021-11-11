package caliban

package object tools {
  val supportedScalars = Set("Int", "Float", "Double", "Long", "Unit", "String", "Boolean", "BigInt", "BigDecimal")

  val reservedKeywords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
    "_"
  )

  val caseClassReservedFields =
    Set("wait", "notify", "toString", "notifyAll", "hashCode", "getClass", "finalize", "equals", "clone")

  val tripleQuotes = "\"\"\""
  val doubleQuotes = "\""
}
