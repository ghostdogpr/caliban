package caliban.gateway

import sttp.model.Header
import zio.Duration

/**
 * Bounds and static headers used while acquiring one remote schema.
 */
final class SchemaAcquisition private (
  private[gateway] val timeout: Duration,
  private[gateway] val maxResponseBytes: Int,
  private[gateway] val maxParsingDepth: Int,
  private[gateway] val headers: List[Header]
) {

  /**
   * Sets the maximum duration of one schema acquisition.
   */
  def withTimeout(value: Duration): SchemaAcquisition =
    new SchemaAcquisition(value, maxResponseBytes, maxParsingDepth, headers)

  /**
   * Sets the maximum response body size accepted during schema acquisition.
   */
  def withMaxResponseBytes(value: Int): SchemaAcquisition =
    new SchemaAcquisition(timeout, value, maxParsingDepth, headers)

  /**
   * Sets the maximum JSON or GraphQL nesting depth parsed during schema acquisition.
   */
  def withMaxParsingDepth(value: Int): SchemaAcquisition =
    new SchemaAcquisition(timeout, maxResponseBytes, value, headers)

  /**
   * Sets static headers sent only during schema acquisition.
   */
  def withHeaders(values: Header*): SchemaAcquisition =
    new SchemaAcquisition(timeout, maxResponseBytes, maxParsingDepth, values.toList)

  private[gateway] def diagnostics: List[String] = {
    val protectedHeaders = headers.collect {
      case header if SchemaAcquisition.ProtocolHeaders.contains(header.name.toLowerCase(java.util.Locale.ROOT)) =>
        s"Schema acquisition header '${header.name}' is owned by the GraphQL transport."
    }
    val timeoutError     =
      if (timeout.compareTo(Duration.Zero) <= 0 || timeout.compareTo(Duration.Infinity) >= 0)
        List("Schema acquisition timeout must be finite and positive.")
      else Nil
    val responseError    =
      if (maxResponseBytes <= 0) List("Schema acquisition maxResponseBytes must be positive.") else Nil
    val parsingError     =
      if (maxParsingDepth <= 0) List("Schema acquisition maxParsingDepth must be positive.") else Nil

    timeoutError ::: responseError ::: parsingError ::: protectedHeaders
  }
}

object SchemaAcquisition {
  private val ProtocolHeaders =
    Set("accept", "content-encoding", "content-length", "content-type", "host", "transfer-encoding")

  /**
   * The default finite schema-acquisition bounds.
   */
  val default: SchemaAcquisition =
    new SchemaAcquisition(
      timeout = zio.Duration.fromSeconds(10),
      maxResponseBytes = 16 * 1024 * 1024,
      maxParsingDepth = 128,
      headers = Nil
    )
}
