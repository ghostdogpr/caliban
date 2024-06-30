package caliban.tools

import caliban._
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.{ Task, ZIO }

import java.io.File

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
    headers: Option[List[Options.Header]],
    supportIsRepeatable: Boolean
  ) extends SchemaLoader {
    override def load: Task[Document] =
      IntrospectionClient.introspect(url, headers, supportIsRepeatable).provideLayer(HttpClientZioBackend.layer())
  }

  def fromCaliban[R](api: GraphQL[R]): SchemaLoader = FromCaliban(api)
  def fromDocument(doc: Document): SchemaLoader     = FromDocument(doc)
  def fromFile(path: String): SchemaLoader          = FromFile(path)
  def fromFile(file: File): SchemaLoader            = FromFile(file.getAbsolutePath)
  def fromString(schema: String): SchemaLoader      = FromString(schema)
  def fromIntrospection(
    url: String,
    headers: Option[List[Options.Header]],
    supportIsRepeatable: Boolean = true
  ): SchemaLoader =
    FromIntrospection(url, headers, supportIsRepeatable)
}
