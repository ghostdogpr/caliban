package caliban

import caliban.wrappers.Caching
import zio.stream.{ ZChannel, ZPipeline }
import zio.{ Cause, Chunk }

private[caliban] object HttpUtils {

  object DeferMultipart {
    private val Newline        = "\r\n"
    private val ContentType    = "Content-Type: application/json; charset=utf-8"
    private val SubHeader      = s"$Newline$ContentType$Newline$Newline"
    private val Boundary       = "---"
    private val BoundaryHeader = "-"
    private val DeferSpec      = "20220824"

    val InnerBoundary = s"$Newline$Boundary$SubHeader"
    val EndBoundary   = s"$Newline-----$Newline"

    val DeferHeaderParams: Map[String, String] = Map("boundary" -> BoundaryHeader, "deferSpec" -> DeferSpec)

    def createPipeline[E](resp: GraphQLResponse[E]): ZPipeline[Any, Throwable, ResponseValue, ResponseValue] =
      ZPipeline.fromChannel {
        lazy val reader: ZChannel[Any, Throwable, Chunk[ResponseValue], Any, Throwable, Chunk[ResponseValue], Any] =
          ZChannel.readWithCause(
            (in: Chunk[ResponseValue]) =>
              in.headOption match {
                case Some(value) =>
                  ZChannel.write(in.updated(0, resp.copy(data = value).toResponseValue)) *>
                    ZChannel.identity[Throwable, Chunk[ResponseValue], Any]
                case None        => reader
              },
            (cause: Cause[Throwable]) => ZChannel.failCause(cause),
            (_: Any) => ZChannel.unit
          )

        reader
      }
  }

  def computeCacheDirective(extensions: Option[ResponseValue.ObjectValue]): Option[String] =
    extensions
      .flatMap(_.fields.collectFirst { case (Caching.DirectiveName, ResponseValue.ObjectValue(fields)) =>
        fields.collectFirst { case ("httpHeader", Value.StringValue(cacheHeader)) => cacheHeader }
      }.flatten)

  final class AcceptsGqlEncodings(header0: Option[String]) {
    private val isEmpty     = header0.isEmpty
    private val length      = if (isEmpty) 0 else header0.get.length
    private lazy val header = if (isEmpty) "" else header0.get.toLowerCase

    /**
     * NOTE: From  1st January 2025 this should be changed to `true` as the default
     *
     * @see [[https://graphql.github.io/graphql-over-http/draft/#sec-Legacy-watershed]]
     */
    def graphQLJson: Boolean = length >= 33 && header.contains("application/graphql-response+json")

    def serverSentEvents: Boolean = length >= 17 && header.contains("text/event-stream")
  }
}
