package caliban.gateway

import caliban.client.GraphQLResponseError
import caliban.CalibanError.ParsingError
import sttp.model.StatusCode
import zio.Duration

import scala.util.control.NoStackTrace

private[gateway] trait GatewayDiagnosticError extends NoStackTrace with Product with Serializable {
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
sealed trait GatewayBuildError extends GatewayDiagnosticError

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

}

/**
 * Identifies a build failure belonging to one subgraph.
 */
final case class SubgraphError(name: String, error: SubgraphBuildError) {
  def diagnostics: List[String] =
    error.diagnostics.map { message =>
      val prefix = s"[$name]"
      if (message.startsWith(prefix)) message else s"$prefix $message"
    }
}

/**
 * A failure that prevented a subgraph from being loaded.
 */
sealed trait SubgraphBuildError extends GatewayDiagnosticError

object SubgraphBuildError {

  /**
   * The subgraph has invalid configuration.
   */
  final case class InvalidConfiguration(errors: List[String]) extends SubgraphBuildError {
    override val diagnostics: List[String] = errors
  }

  /**
   * A remote subgraph was loaded without an available HTTP transport.
   */
  case object RemoteTransportUnavailable extends SubgraphBuildError {
    override val diagnostics: List[String] = List("Remote GraphQL transport is unavailable.")
  }

  /**
   * The schema document could not be converted into an executable schema.
   */
  final case class InvalidSchema(error: caliban.CalibanError.ValidationError)
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
 * A failure while obtaining a remote subgraph's schema.
 */
sealed trait SchemaAcquisitionError extends SubgraphBuildError

object SchemaAcquisitionError {

  /**
   * A provided SDL document could not be parsed.
   */
  final case class InvalidProvidedSchema(error: ParsingError) extends SchemaAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List(error.getMessage)
  }

  /**
   * The remote acquisition request failed before a response was received.
   */
  final case class RequestFailed(error: Throwable) extends SchemaAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List("Schema acquisition request failed.")
  }

  /**
   * Schema acquisition did not finish within its configured timeout.
   */
  final case class TimedOut(timeout: Duration) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition timed out after $timeout.")
  }

  /**
   * The response exceeded the configured byte limit.
   */
  final case class ResponseTooLarge(maxBytes: Int) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition response exceeded $maxBytes bytes.")
  }

  /**
   * The response status or media type was not accepted for schema acquisition.
   */
  final case class UnexpectedResponse(status: StatusCode, contentType: Option[String]) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = {
      val mediaType = contentType.fold("without a media type")(value => s"with media type '$value'")
      List(s"Schema acquisition response had status ${status.code} $mediaType.")
    }
  }

  /**
   * The introspection response could not be decoded.
   */
  final case class IntrospectionResponseDecodingFailed(error: Throwable)
      extends SchemaAcquisitionError
      with GatewayCausedError {
    override val diagnostics: List[String] = List("Introspection response could not be decoded.")
  }

  /**
   * Introspection completed with GraphQL errors.
   */
  final case class IntrospectionErrors(errors: List[GraphQLResponseError]) extends SchemaAcquisitionError {
    override val diagnostics: List[String] =
      List(s"Introspection failed: ${errors.map(_.render(includeExtensions = false)).mkString("; ")}.")
  }

  /**
   * The Federation response body was not valid JSON.
   */
  final case class FederationResponseDecodingFailed(error: Throwable)
      extends SchemaAcquisitionError
      with GatewayCausedError {
    override val diagnostics: List[String] = List("Federation service response could not be decoded.")
  }

  /**
   * The Federation `_service` response did not contain usable SDL.
   */
  final case class InvalidFederationResponse(reason: InvalidFederationResponse.Reason) extends SchemaAcquisitionError {
    override val diagnostics: List[String] =
      List(s"Federation service response was invalid: ${reason.description}.")
  }

  object InvalidFederationResponse {
    sealed trait Reason extends Product with Serializable {
      def description: String
    }

    case object ExpectedResponseObject extends Reason {
      override val description: String = "the top-level response was not an object"
    }

    case object InvalidErrors extends Reason {
      override val description: String = "the 'errors' field was malformed"
    }

    case object MissingData extends Reason {
      override val description: String = "the 'data' field was missing or not an object"
    }

    case object MissingService extends Reason {
      override val description: String = "the '_service' field was missing or not an object"
    }

    case object MissingSdl extends Reason {
      override val description: String = "the 'sdl' field was missing or not a string"
    }
  }

  /**
   * The Federation `_service` operation completed with GraphQL errors.
   */
  final case class FederationErrors(errors: List[caliban.CalibanError]) extends SchemaAcquisitionError {
    override val diagnostics: List[String] =
      List(s"Federation service returned GraphQL errors: ${errors.map(_.getMessage).mkString("; ")}")
  }

  /**
   * The Federation service SDL could not be parsed.
   */
  final case class InvalidFederationSchema(error: ParsingError) extends SchemaAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] =
      List(s"Federation service schema could not be parsed: ${error.getMessage}")
  }

  /**
   * JSON or GraphQL nesting exceeded the configured parsing-depth limit.
   */
  final case class ParsingDepthExceeded(maxDepth: Int) extends SchemaAcquisitionError {
    override val diagnostics: List[String] = List(s"Schema acquisition parsing depth exceeded $maxDepth.")
  }
}
