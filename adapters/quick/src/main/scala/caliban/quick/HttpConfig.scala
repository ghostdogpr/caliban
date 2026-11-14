package caliban.quick

/**
 * Configuration settings for HTTP requests and responses.
 *
 * @param maxRequestBodyBytes The maximum number of bytes materialized from one JSON or GraphQL request body.
 * @param maxUploadBodyBytes The maximum number of bytes materialized from one multipart upload body.
 * @param maxResponseBodyBytes The maximum number of bytes materialized for one JSON response body.
 * @param partialSuccessStatus Reply `294 Partial Success` instead of `200` when an `application/graphql-response+json`
 *                             result contains both data and errors.
 * @param jsonClientStatusCodes Reply request errors to clients that only accept `application/json` with the same
 *                              4xx status codes as `application/graphql-response+json` clients, instead of `200`.
 *                              Non-2xx responses then use `Content-Type: application/graphql-response+json`.
 */
case class HttpConfig(
  maxRequestBodyBytes: Int,
  maxUploadBodyBytes: Int,
  maxResponseBodyBytes: Int,
  partialSuccessStatus: Boolean = false,
  jsonClientStatusCodes: Boolean = false
) {
  def withMaxRequestBodyBytes(value: Int): HttpConfig =
    copy(maxRequestBodyBytes = value)

  def withMaxUploadBodyBytes(value: Int): HttpConfig =
    copy(maxUploadBodyBytes = value)

  def withMaxResponseBodyBytes(value: Int): HttpConfig =
    copy(maxResponseBodyBytes = value)

  def withPartialSuccessStatus(value: Boolean): HttpConfig =
    copy(partialSuccessStatus = value)

  def withJsonClientStatusCodes(value: Boolean): HttpConfig =
    copy(jsonClientStatusCodes = value)
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
