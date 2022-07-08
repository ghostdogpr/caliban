package caliban.tools

import caliban.GraphQL
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import zio.{ Task, UIO }

trait SchemaLoader {
  def load: Task[Document]
}

object SchemaLoader {
  case class FromCaliban private[SchemaLoader] (api: GraphQL[_]) extends SchemaLoader {
    override def load: Task[Document] = UIO(api.toDocument)
  }
  case class FromDocument private[SchemaLoader] (doc: Document)  extends SchemaLoader {
    override def load: Task[Document] = UIO(doc)
  }
  case class FromFile private[SchemaLoader] (path: String)       extends SchemaLoader {
    override def load: Task[Document] =
      Task(scala.io.Source.fromFile(path))
        .bracket(f => UIO(f.close()), f => Task(f.mkString))
        .flatMap(Parser.parseQuery)
  }
  case class FromString private[SchemaLoader] (schema: String)   extends SchemaLoader {
    override def load: Task[Document] = Parser.parseQuery(schema)
  }
  case class FromIntrospection private[SchemaLoader] (url: String, headers: Option[List[Options.Header]])
      extends SchemaLoader {
    override def load: Task[Document] = IntrospectionClient.introspect(url, headers)
  }

  def fromCaliban[R](api: GraphQL[R]): SchemaLoader                                       = FromCaliban(api)
  def fromDocument(doc: Document): SchemaLoader                                           = FromDocument(doc)
  def fromFile(path: String): SchemaLoader                                                = FromFile(path)
  def fromString(schema: String): SchemaLoader                                            = FromString(schema)
  def fromIntrospection(url: String, headers: Option[List[Options.Header]]): SchemaLoader =
    FromIntrospection(url, headers)
}
