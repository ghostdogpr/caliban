package caliban.reporting.client

import caliban.client._
import caliban.client.__Value._

final case class SchemaReport(
  bootId: String,
  coreSchemaHash: String,
  graphRef: String,
  libraryVersion: Option[String] = None,
  platform: Option[String] = None,
  runtimeVersion: Option[String] = None,
  serverId: Option[String] = None,
  userVersion: Option[String] = None
)
object SchemaReport {
  implicit val encoder: ArgEncoder[SchemaReport] = new ArgEncoder[SchemaReport] {
    override def encode(value: SchemaReport): __Value =
      __ObjectValue(
        List(
          "bootId"         -> implicitly[ArgEncoder[String]].encode(value.bootId),
          "coreSchemaHash" -> implicitly[ArgEncoder[String]].encode(value.coreSchemaHash),
          "graphRef"       -> implicitly[ArgEncoder[String]].encode(value.graphRef),
          "libraryVersion" -> value.libraryVersion.fold(__NullValue: __Value)(value =>
            implicitly[ArgEncoder[String]].encode(value)
          ),
          "platform"       -> value.platform.fold(__NullValue: __Value)(value =>
            implicitly[ArgEncoder[String]].encode(value)
          ),
          "runtimeVersion" -> value.runtimeVersion.fold(__NullValue: __Value)(value =>
            implicitly[ArgEncoder[String]].encode(value)
          ),
          "serverId"       -> value.serverId.fold(__NullValue: __Value)(value =>
            implicitly[ArgEncoder[String]].encode(value)
          ),
          "userVersion"    -> value.userVersion.fold(__NullValue: __Value)(value =>
            implicitly[ArgEncoder[String]].encode(value)
          )
        )
      )
  }
}

