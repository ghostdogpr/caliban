import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait foo_1 extends scala.Product with scala.Serializable { def value: String }
  object foo_1 {
    case object A extends foo_1 { val value: String = "A" }
    case object B extends foo_1 { val value: String = "B" }

    implicit val decoder: ScalarDecoder[foo_1] = {
      case __StringValue("A") => Right(foo_1.A)
      case __StringValue("B") => Right(foo_1.B)
      case other              => Left(DecodingError(s"Can't build foo from input $other"))
    }
    implicit val encoder: ArgEncoder[foo_1]    = {
      case foo_1.A => __EnumValue("A")
      case foo_1.B => __EnumValue("B")
    }

    val values: scala.collection.immutable.Vector[foo_1] = scala.collection.immutable.Vector(A, B)
  }

  type Foo
  object Foo {
    def id: SelectionBuilder[Foo, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("id", OptionOf(Scalar()))
  }

}
