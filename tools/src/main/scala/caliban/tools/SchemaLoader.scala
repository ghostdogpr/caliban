package caliban.tools

import caliban._
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import sttp.client4.httpclient.zio.HttpClientZioBackend
import zio.{ Task, ZIO }

trait SchemaLoader {
  def load: Task[Document]
}

object SchemaLoader {
  case class FromCaliban private[SchemaLoader] (api: GraphQL[_]) extends SchemaLoader {
    override def load: Task[Document] = ZIO.succeed(api.toDocument)
  }
  case class FromDocument private[SchemaLoader] (doc: Document)  extends SchemaLoader {
    override def load: Task[Document] = ZIO.succeed(doc)
  }
  case class FromFile private[SchemaLoader] (path: String)       extends SchemaLoader {
    override def load: Task[Document] = ZIO.blocking {
      ZIO
        .attempt(scala.io.Source.fromFile(path))
        .acquireReleaseWithAuto(f => ZIO.attempt(f.mkString))
    }.map(Parser.parseQuery).absolve
  }
  case class FromString private[SchemaLoader] (schema: String)   extends SchemaLoader {
    override def load: Task[Document] = ZIO.fromEither(Parser.parseQuery(schema))
  }

  case class FromIntrospection private[SchemaLoader] (
    url: String,
    headers: Option[List[Header]],
    config: IntrospectionClient.Config
  ) extends SchemaLoader {
    override def load: Task[Document] =
      IntrospectionClient.introspect(url, headers, config).provideLayer(HttpClientZioBackend.layer())
  }

  def fromCaliban[R](api: GraphQL[R]): SchemaLoader = FromCaliban(api)
  def fromDocument(doc: Document): SchemaLoader     = FromDocument(doc)
  def fromFile(path: String): SchemaLoader          = FromFile(path)
  def fromString(schema: String): SchemaLoader      = FromString(schema)

  def fromIntrospection(
    url: String,
    headers: Option[List[Header]]
  ): SchemaLoader =
    fromIntrospectionWith(url, headers)(identity)

  def fromIntrospection(
    url: String,
    headers: Option[List[Header]],
    config: IntrospectionClient.Config
  ): SchemaLoader =
    FromIntrospection(url, headers, config)

  def fromIntrospectionWith(
    url: String,
    headers: Option[List[Header]]
  )(config: IntrospectionClient.Config => IntrospectionClient.Config): SchemaLoader =
    fromIntrospection(url, headers, config(IntrospectionClient.Config.default))
}
