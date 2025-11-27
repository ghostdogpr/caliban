import Types._

import caliban.schema.Annotations._

object Types {

  final case class Bar(message: String)    extends Error with AllErrors derives caliban.schema.Schema.SemiAuto
  final case class Foo(message: String)    extends Error with AllErrors derives caliban.schema.Schema.SemiAuto
  final case class FooBar(message: String) extends Error with AllErrors derives caliban.schema.Schema.SemiAuto

  sealed trait AllErrors extends scala.Product with scala.Serializable derives caliban.schema.Schema.SemiAuto
  @GQLInterface
  sealed trait Error extends scala.Product with scala.Serializable derives caliban.schema.Schema.SemiAuto {
    @GQLDescription("description")
    def message: String
  }

}

object Operations {

  final case class Query(
    errorInterface: zio.UIO[Error],
    errorUnion: zio.UIO[AllErrors]
  ) derives caliban.schema.Schema.SemiAuto

}
