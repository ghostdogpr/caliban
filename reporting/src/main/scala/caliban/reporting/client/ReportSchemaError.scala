package caliban.reporting.client

import caliban.client.FieldBuilder._
import caliban.client._

object ReportSchemaError {
  def code: SelectionBuilder[ReportSchemaError, ReportSchemaErrorCode] =
    _root_.caliban.client.SelectionBuilder.Field("code", Scalar())
  def inSeconds: SelectionBuilder[ReportSchemaError, Int]              =
    _root_.caliban.client.SelectionBuilder.Field("inSeconds", Scalar())
  def message: SelectionBuilder[ReportSchemaError, String]             =
    _root_.caliban.client.SelectionBuilder.Field("message", Scalar())
  def withCoreSchema: SelectionBuilder[ReportSchemaError, Boolean]     =
    _root_.caliban.client.SelectionBuilder.Field("withCoreSchema", Scalar())
}
