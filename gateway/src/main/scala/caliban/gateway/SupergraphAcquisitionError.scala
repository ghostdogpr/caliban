package caliban.gateway

import caliban.CalibanError.ParsingError
import sttp.model.StatusCode
import zio.Duration

sealed trait SupergraphAcquisitionError extends GatewayDiagnosticError

object SupergraphAcquisitionError {
  final case class InvalidSupergraphSchema(error: ParsingError)
      extends SupergraphAcquisitionError
      with GatewayCausedError {
    override val diagnostics: List[String] = List(error.getMessage)
  }

  final case class RequestFailed(error: Throwable) extends SupergraphAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List("Supergraph acquisition request failed")
  }

  final case class TimedOut(timeout: Duration)                                 extends SupergraphAcquisitionError                         {
    override val diagnostics: List[String] = List(s"Supergraph schema acquisition timed out after $timeout")
  }
  final case class ResponseTooLarge(maxBytes: Int)                             extends SupergraphAcquisitionError                         {
    override val diagnostics: List[String] = List(s"Supergraph schema acquisition response exceeded $maxBytes bytes.")
  }
  final case class UnexpectedResponse(status: StatusCode, contentType: Option[String])
      extends SupergraphAcquisitionError {
    override val diagnostics: List[String] = {
      val mediaType = contentType.fold("without a media type")(value => s"with media type '$value'")
      List(s"Supergraph schema acquisition response has status ${status.code} $mediaType.")
    }
  }
  final case class ParsingDepthExceeded(maxDepth: Int)                         extends SupergraphAcquisitionError                         {
    override val diagnostics: List[String] = List(s"Supergraph schema parsing depth exceeded $maxDepth.")
  }
  final case class FileUnreadable(error: Throwable)                            extends SupergraphAcquisitionError with GatewayCausedError {
    override val diagnostics: List[String] = List("Supergraph schema acquisition was unable to read a file.")
  }
  final case class UplinkFetchFailed(code: String)                             extends SupergraphAcquisitionError                         {
    override val diagnostics: List[String] = List(s"Supergraph uplink returned error code '$code'.")
  }
  final case class InvalidUplinkResponse(reason: InvalidUplinkResponse.Reason) extends SupergraphAcquisitionError                         {
    override val diagnostics: List[String] =
      List(s"Uplink response was invalid: ${reason.description}.")
  }
  object InvalidUplinkResponse {
    sealed trait Reason {
      def description: String
    }
    case object MissingData          extends Reason {
      override val description: String = "the 'data' field was missing or not an object"
    }
    case object MissingRouterConfig  extends Reason {
      override val description: String = "the 'router' config was missing or not an object"
    }
    case object UnknownTypename      extends Reason {
      override val description: String = "the '__typename' field was unknown"
    }
    case object MissingSupergraphSdl extends Reason {
      override val description: String = "the 'supergraphSdl' config was missing or not a string"
    }
    case object MissingId            extends Reason {
      override val description: String = "the 'id' field was missing"
    }
    case object DecodingFailed       extends Reason {
      override val description: String = "the response could not be decoded"
    }
  }
}
