package caliban

/**
 * Opt-in HTTP status codes from the GraphQL over HTTP spec, used by the HTTP adapters.
 *
 * @param partialSuccess Reply `294 Partial Success` instead of `200` when an `application/graphql-response+json`
 *                       result contains both data and errors.
 * @param jsonClientStatusCodes Reply request errors to clients that only accept `application/json` with the same
 *                              4xx status codes as `application/graphql-response+json` clients, instead of `200`.
 *                              Non-2xx responses then use `Content-Type: application/graphql-response+json`.
 */
case class StatusCodeConfig(partialSuccess: Boolean, jsonClientStatusCodes: Boolean) {
  def withPartialSuccess(value: Boolean): StatusCodeConfig =
    copy(partialSuccess = value)

  def withJsonClientStatusCodes(value: Boolean): StatusCodeConfig =
    copy(jsonClientStatusCodes = value)
}

object StatusCodeConfig {

  def default: StatusCodeConfig = StatusCodeConfig(partialSuccess = false, jsonClientStatusCodes = false)

  def spec: StatusCodeConfig = StatusCodeConfig(partialSuccess = true, jsonClientStatusCodes = true)
}
