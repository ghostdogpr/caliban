package caliban.reporting.client

import caliban.client.FieldBuilder._
import caliban.client._

object ReportSchemaResult {
  def inSeconds: SelectionBuilder[ReportSchemaResult, Int]          =
    _root_.caliban.client.SelectionBuilder.Field("inSeconds", Scalar())
  def withCoreSchema: SelectionBuilder[ReportSchemaResult, Boolean] =
    _root_.caliban.client.SelectionBuilder.Field("withCoreSchema", Scalar())
}
