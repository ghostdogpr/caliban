package caliban.quick

/**
 * Configuration settings for HTTP requests and responses.
 *
 * @param maxRequestBodyBytes The maximum number of bytes materialized from one JSON or GraphQL request body.
 * @param maxUploadBodyBytes The maximum number of bytes materialized from one multipart upload body.
 * @param maxResponseBodyBytes The maximum number of bytes materialized for one JSON response body.
 */
case class HttpConfig(
  maxRequestBodyBytes: Int,
  maxUploadBodyBytes: Int,
  maxResponseBodyBytes: Int
) {
  def withMaxRequestBodyBytes(value: Int): HttpConfig =
    copy(maxRequestBodyBytes = value)

  def withMaxUploadBodyBytes(value: Int): HttpConfig =
    copy(maxUploadBodyBytes = value)

  def withMaxResponseBodyBytes(value: Int): HttpConfig =
    copy(maxResponseBodyBytes = value)
}

object HttpConfig {

  /**
   * One megabyte for request bodies, 16 megabytes for upload bodies and 16 megabytes for response bodies.
   */
  def default: HttpConfig = HttpConfig(
    maxRequestBodyBytes = 1024 * 1024,
    maxUploadBodyBytes = 16 * 1024 * 1024,
    maxResponseBodyBytes = 16 * 1024 * 1024
  )
}
