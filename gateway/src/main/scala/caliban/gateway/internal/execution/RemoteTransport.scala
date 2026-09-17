package caliban.gateway.internal.execution

/**
 * Shared HTTP body limits and media-type handling for schema acquisition and remote execution.
 * The structural scan bounds decoding work; the JSON decoder remains responsible for syntax validation.
 */
private[gateway] object RemoteTransport {
  final case class BoundedBody(bytes: Array[Byte], limitExceeded: Boolean)

  sealed trait JsonStructureLimit
  case object JsonDepthExceeded  extends JsonStructureLimit
  case object JsonTokensExceeded extends JsonStructureLimit

  def mediaType(contentType: Option[String]): Option[String] =
    contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))

  def validateJsonStructure(bytes: Array[Byte], maxDepth: Int, maxTokens: Int): Either[JsonStructureLimit, Unit] = {
    val length   = bytes.length
    var depth    = 0
    var index    = 0
    var escaped  = false
    var string   = false
    var tokens   = 0
    var previous = 0.toByte

    while (index < length) {
      val current = bytes(index)
      if (string) {
        if (escaped) escaped = false
        else if (current == '\\') escaped = true
        else if (current == '"') {
          string = false
          previous = current
        }
      } else
        current match {
          case '"'                                     =>
            string = true
            tokens += 1
          case '{' | '['                               =>
            depth += 1
            tokens += 1
            previous = current
            if (depth > maxDepth) return Left(JsonDepthExceeded)
          case '}' | ']'                               =>
            depth -= 1
            previous = current
          case value if isScalarStart(value, previous) =>
            tokens += 1
            previous = current
          case ' ' | '\t' | '\n' | '\r'                => ()
          case _                                       => previous = current
        }

      if (tokens > maxTokens) return Left(JsonTokensExceeded)
      index += 1
    }

    Right(())
  }

  private def isScalarStart(value: Byte, previous: Byte): Boolean = {
    val scalar   = value == '-' || value >= '0' && value <= '9' || value == 't' || value == 'f' || value == 'n'
    val boundary = previous == 0 || previous == '[' || previous == ',' || previous == ':'
    scalar && boundary
  }
}
