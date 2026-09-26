package caliban.gateway

import caliban.CalibanError
import caliban.CalibanError.ParsingError
import zio.http.Status
import zio.Duration

import scala.util.control.NoStackTrace

private[gateway] trait GatewayError extends NoStackTrace with Product with Serializable {
  def diagnostics: List[String]

  override final def getMessage: String = diagnostics.mkString("\n")
}

private[gateway] trait GatewayCausedError { self: Throwable =>
  def error: Throwable

  override final def getCause: Throwable = error
}

/**
 * Indicates that a [[Gateway]] could not be built.
 *
 * The cases identify broad failure categories. [[diagnostics]] is a rendered view intended for logs and command-line
 * output; callers can pattern match on the cases and their fields when they need structured error handling.
 */
sealed trait GatewayBuildError extends GatewayError

object GatewayBuildError {

  /**
   * The gateway or one of its subgraphs has invalid configuration, including missing enforcement of the @authenticated
   * or @requiresScopes directives.
   */
  final case class InvalidConfiguration(errors: List[String]) extends GatewayBuildError {
    override val diagnostics: List[String] = errors
  }

  /**
   * The HTTP transport required by remote subgraphs could not be initialized.
   */
  final case class TransportInitializationFailed(error: Throwable) extends GatewayBuildError with GatewayCausedError {
    override val diagnostics: List[String] =
      List("Unable to initialize the remote GraphQL transport.")
  }

  /**
   * One or more subgraphs could not be loaded.
   */
  final case class SubgraphLoadingFailed(errors: List[SubgraphError]) extends GatewayBuildError {
    override val diagnostics: List[String] = errors.flatMap(_.diagnostics)
  }

  /**
   * The loaded subgraph schemas could not be composed.
   */
  final case class SchemaCompositionFailed(errors: List[String]) extends GatewayBuildError {
    override val diagnostics: List[String] = errors
  }

  final case class SupergraphAcquisitionFailed(error: SupergraphAcquisitionError)
      extends GatewayBuildError
      with GatewayCausedError {
    override val diagnostics: List[String] = error.diagnostics.map(message => s"[supergraph] $message")
  }

  final case class SupergraphDecompositionFailed(errors: List[String]) extends GatewayBuildError {
    override val diagnostics: List[String] = errors
  }

}

/**
 * Identifies a build failure belonging to one subgraph.
 */
final case class SubgraphError(name: String, error: SubgraphBuildError) {

  /**
   * The error's diagnostics, each prefixed with the subgraph name.
   */
  def diagnostics: List[String] = {
    val prefix = s"[$name]"
    error.diagnostics.map(message => if (message.startsWith(prefix)) message else s"$prefix $message")
  }
}

/**
 * A failure that prevented a subgraph from being loaded.
 */
sealed trait SubgraphBuildError extends GatewayError

object SubgraphBuildError {

  /**
   * The subgraph has invalid configuration.
   */
  final case class InvalidConfiguration(errors: List[String]) extends SubgraphBuildError {
    override val diagnostics: List[String] = errors
  }

  /**
   * The parsed schema document failed validation while preparing an executable subgraph.
   */
  final case class SchemaValidationFailed(error: CalibanError.ValidationError)
      extends SubgraphBuildError
      with GatewayCausedError {
    override val diagnostics: List[String] = List(error.getMessage)
  }

  /**
   * Schema transformations or coordinate mappings were invalid.
   */
  final case class InvalidTransformations(errors: List[String]) extends SubgraphBuildError {
    override val diagnostics: List[String] = errors
  }
}

/**
 * A failure while obtaining or parsing a remote subgraph's schema, including supplied SDL.
 */
sealed trait SubgraphAcquisitionError extends SubgraphBuildError

object SubgraphAcquisitionError {

  /**
   * Introspection completed with GraphQL errors.
   */
  final case class IntrospectionErrors(errors: List[CalibanError]) extends SubgraphAcquisitionError {
    override val diagnostics: List[String] =
      List(s"Introspection failed: ${errors.map(_.getMessage).mkString("; ")}")
  }

  /**
   * The Federation `_service` operation completed with GraphQL errors.
   */
  final case class FederationErrors(errors: List[CalibanError]) extends SubgraphAcquisitionError {
    override val diagnostics: List[String] =
      List(s"Federation service returned GraphQL errors: ${errors.map(_.getMessage).mkString("; ")}")
  }
}

/**
 * A failure while reading, fetching, or parsing a supergraph.
 */
sealed trait SupergraphAcquisitionError extends GatewayError

object SupergraphAcquisitionError {
  final case class FileReadFailed(error: Throwable) extends SupergraphAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List("Supergraph schema acquisition was unable to read a file.")
  }

  final case class UplinkFetchFailed(code: String) extends SupergraphAcquisitionError {
    override val diagnostics: List[String] = List(s"Supergraph uplink returned error code '$code'.")
  }
}

/**
 * A failure shared by subgraph schema and supergraph acquisition: the request failed, timed out, or got an
 * oversized, unexpected, undecodable, malformed, or too deeply nested response, or the SDL did not parse.
 */
sealed trait SchemaAcquisitionError extends SubgraphAcquisitionError with SupergraphAcquisitionError

object SchemaAcquisitionError {
  final case class SchemaParsingFailed(error: ParsingError) extends SchemaAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List(error.getMessage)
  }

  final case class RequestFailed(error: Throwable) extends SchemaAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List("Schema acquisition request failed.")
  }

  final case class TimedOut(timeout: Duration) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition timed out after $timeout.")
  }

  final case class ResponseTooLarge(maxBytes: Int) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition response exceeded $maxBytes bytes.")
  }

  final case class UnexpectedResponse(status: Status, contentType: Option[String]) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = {
      val mediaType = contentType.fold("without a media type")(value => s"with media type '$value'")
      List(s"Schema acquisition response had status ${status.code} $mediaType.")
    }
  }

  final case class ResponseDecodingFailed(error: Throwable) extends SchemaAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List("Schema acquisition response could not be decoded.")
  }

  final case class InvalidResponse(path: String) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition response was invalid at '$path'.")
  }

  final case class ParsingDepthExceeded(maxDepth: Int) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition parsing depth exceeded $maxDepth.")
  }
}
