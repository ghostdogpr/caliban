package caliban.tools.stitching

import zio._
import zio.query._

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import caliban.schema._
import caliban.introspection.adt._

import zio.query._

import sttp.client3._
import sttp.client3.circe._
import sttp.client3.asynchttpclient.zio._

case class RemoteResolver(typeMap: Map[String, __Type], apiURL: String) {
  def remoteResolver[R, A](typeName: String)(
    prepare: (A, caliban.execution.Field) => caliban.execution.Field,
    beforeSend: RequestT[Identity, Either[String, String], Any] => RequestT[
      Identity,
      Either[String, String],
      Any
    ] = identity
  ) = new PartialSchema[SttpClient, R, A] {

    def resolve(a: A, args: caliban.execution.Field): ZIO[SttpClient, CalibanError, ResponseValue] = {
      val q = RemoteQuery(prepare(a, args)).toGraphQLRequest

      println(s"query: $q")

      val encoder = implicitly[io.circe.Encoder[GraphQLRequest]]
      val decoder = implicitly[io.circe.Decoder[GraphQLResponse[CalibanError]]]

      println(s"Hi: $encoder $decoder")

      val req = beforeSend(basicRequest.post(uri"$apiURL"))
        .body(q)
        .response(asJson[GraphQLResponse[CalibanError]])

      (for {
        res  <- send(req)
        body <- ZIO.fromEither(res.body)
      } yield body.data).mapError(e => CalibanError.ExecutionError(e.toString()))
    }

    def toType(isInput: Boolean, isSubscription: Boolean): __Type = typeMap(typeName)
  }
}

object RemoteResolver {
  def fromSchema(schema: __Schema, apiURL: String): RemoteResolver = {
    val typeMap = schema.types
      .collect({ t =>
        (t.name) match {
          case Some(name) => name -> t
        }
      })
      .toMap

    RemoteResolver(typeMap, apiURL)
  }
}

trait PartialSchema[R0, R, A] { self =>
  def toType(isInput: Boolean, isSubscription: Boolean): __Type

  def resolve(value: A, args: caliban.execution.Field): ZIO[R0, CalibanError, ResponseValue]

  def provide[R1 <: R0](env: R1): Schema[R, A] = new Schema[R, A] {
    def resolve(value: A): Step[R] =
      Step.MetadataFunctionStep { (args: caliban.execution.Field) =>
        Step.QueryStep(ZQuery.fromEffect(self.resolve(value, args).map(Step.PureStep(_)).provide(env)))
      }

    protected def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      self.toType(isInput, isSubscription)
  }
}
