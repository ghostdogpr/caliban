package caliban.gateway.internal

import zio.http.Status

/**
 * Shared HTTP body limits and media-type handling for schema acquisition and remote execution.
 * The structural scan bounds decoding work; the JSON decoder remains responsible for syntax validation.
 */
private[gateway] object RemoteTransport {
  final case class BoundedBody(bytes: Array[Byte], limitExceeded: Boolean)

  final val GraphQLResponseJson = "application/graphql-response+json"
  final val Json                = "application/json"

  def mediaType(contentType: Option[String]): Option[String] =
    contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))

  // GraphQL response JSON can carry errors on non-success statuses; ordinary JSON requires success.
  def isJsonResponse(status: Status, contentType: Option[String]): Boolean = {
    val media = mediaType(contentType)
    media.contains(GraphQLResponseJson) || status.isSuccess && media.contains(Json)
  }

  def withinJsonDepth(bytes: Array[Byte], maxDepth: Int): Boolean = {
    val length  = bytes.length
    var depth   = 0
    var index   = 0
    var escaped = false
    var string  = false

    while (index < length) {
      val current = bytes(index)
      if (string) {
        if (escaped) escaped = false
        else if (current == '\\') escaped = true
        else if (current == '"') string = false
      } else if (current == '"') string = true
      else if (current == '{' || current == '[') {
        depth += 1
        if (depth > maxDepth) return false
      } else if (current == '}' || current == ']') depth -= 1
      index += 1
    }

    true
  }
}
