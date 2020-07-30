package caliban

import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller

/**
 * Very simple json backend adapter that uses raw strings to avoid specific AST dependencies in the interface
 */
trait JsonBackend {
  def parseHttpRequest(
    query: Option[String],
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest]
  def encodeGraphQLResponse(r: GraphQLResponse[Any]): String

  def parseWSMessage(text: String): Either[Throwable, WSMessage]
  def encodeWSResponse[E](id: String, data: ResponseValue, errors: List[E]): String
  def encodeWSError(id: String, error: String): String

  def reqUnmarshaller: FromEntityUnmarshaller[GraphQLRequest]

}
