package caliban.reporting.client

import caliban.client.FieldBuilder._
import caliban.client._

object ReportSchemaResponse {
  def inSeconds: SelectionBuilder[ReportSchemaResponse, Int]          =
    _root_.caliban.client.SelectionBuilder.Field("inSeconds", Scalar())
  def withCoreSchema: SelectionBuilder[ReportSchemaResponse, Boolean] =
    _root_.caliban.client.SelectionBuilder.Field("withCoreSchema", Scalar())
}

