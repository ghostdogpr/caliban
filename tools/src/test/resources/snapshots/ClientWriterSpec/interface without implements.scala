import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client._
import caliban.client.__Value._

object Client {

  sealed trait Status extends scala.Product with scala.Serializable { def value: String }
  object Status {
    case object Status1 extends Status { val value: String = "Status1" }

    implicit val decoder: ScalarDecoder[Status] = {
      case __StringValue("Status1") => Right(Status.Status1)
      case other                    => Left(DecodingError(s"Can't build Status from input $other"))
    }
    implicit val encoder: ArgEncoder[Status]    = { case Status.Status1 =>
      __EnumValue("Status1")
    }

    val values: scala.collection.immutable.Vector[Status] = scala.collection.immutable.Vector(Status1)
  }

  type Entity
  object Entity {
    def statusInfo[A](innerSelection: SelectionBuilder[StatusInfo, A]): SelectionBuilder[Entity, A] =
      _root_.caliban.client.SelectionBuilder.Field("statusInfo", Obj(innerSelection))
  }

  type StatusInfo
  object StatusInfo {
    def status: SelectionBuilder[StatusInfo, Status] = _root_.caliban.client.SelectionBuilder.Field("status", Scalar())
  }

}
