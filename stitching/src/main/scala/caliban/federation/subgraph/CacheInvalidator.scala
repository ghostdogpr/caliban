package caliban.federation.subgraph

import caliban.federation.subgraph.CacheInvalidator.InvalidationMethod
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }
import sttp.client4._
import sttp.client4.jsoniter._
import sttp.model.{ HeaderNames, Uri }
import zio.Config.Secret
import zio.{ Tag, Task, ZLayer }

import java.net.URI

trait CacheInvalidator {
  def invalidate(request: InvalidationMethod): Task[Unit] =
    invalidateAll(List(request))

  def invalidateAll(requests: List[InvalidationMethod]): Task[Unit]
}

object CacheInvalidator {
  case class Config(
    sharedSecret: Secret,
    invalidationUri: URI
  )

  object Config {
    val config: zio.Config[Config] =
      (zio.Config.secret("shared-secret") zipWith zio.Config.uri("invalidation-uri"))(Config.apply)
  }

  sealed trait InvalidationMethod

  object InvalidationMethod {
    case class Subgraph(name: String)                              extends InvalidationMethod
    case class Type(subgraph: String, `type`: String)              extends InvalidationMethod
    case class CacheTag(subgraphs: List[String], cacheTag: String) extends InvalidationMethod

    implicit val codec: JsonValueCodec[InvalidationMethod] = JsonCodecMaker.make(
      CodecMakerConfig
        .withDiscriminatorFieldName(Some("kind"))
        .withEncodingOnly(true)
        .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
        .withAdtLeafClassNameMapper(x => JsonCodecMaker.enforce_snake_case(JsonCodecMaker.simpleClassName(x)))
    )
  }

  val live: ZLayer[Config with Backend[Task], Nothing, CacheInvalidator] =
    ZLayer.fromFunction(Live.apply _)

  def configuredWith[A: Tag](f: A => Config): ZLayer[A with Backend[Task], Nothing, CacheInvalidator] =
    ZLayer.fromFunction(f) >>> live

  private case class Live(config: Config, backend: Backend[Task]) extends CacheInvalidator {
    private implicit val listInvalidationCodec: JsonValueCodec[List[InvalidationMethod]] =
      JsonCodecMaker.make

    def invalidateAll(requests: List[InvalidationMethod]): Task[Unit] =
      backend
        .send(
          basicRequest
            .header(HeaderNames.Authorization, config.sharedSecret.stringValue)
            .post(Uri(config.invalidationUri))
            .body(asJson(requests))
            .response(ignore)
        )
        .unit
  }
}
