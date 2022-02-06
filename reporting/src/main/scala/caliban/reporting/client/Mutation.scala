package caliban.reporting.client

import caliban.client.FieldBuilder._
import caliban.client._

object Mutation {

  /**
   * Report a running GraphQL server's schema.
   */
  def reportSchema[A](coreSchema: Option[String] = None, report: SchemaReport)(
    onReportSchemaError: SelectionBuilder[ReportSchemaError, A],
    onReportSchemaResponse: SelectionBuilder[ReportSchemaResponse, A]
  )(implicit
    encoder0: ArgEncoder[Option[String]],
    encoder1: ArgEncoder[SchemaReport]
  ): SelectionBuilder[_root_.caliban.client.Operations.RootMutation, Option[A]] =
    _root_.caliban.client.SelectionBuilder.Field(
      "reportSchema",
      OptionOf(
        ChoiceOf(
          Map("ReportSchemaError" -> Obj(onReportSchemaError), "ReportSchemaResponse" -> Obj(onReportSchemaResponse))
        )
      ),
      arguments = List(
        Argument("coreSchema", coreSchema, "String")(encoder0),
        Argument("report", report, "SchemaReport!")(encoder1)
      )
    )

  /**
   * Report a running GraphQL server's schema.
   */
  def reportSchemaOption[A](coreSchema: Option[String] = None, report: SchemaReport)(
    onReportSchemaError: Option[SelectionBuilder[ReportSchemaError, A]] = None,
    onReportSchemaResponse: Option[SelectionBuilder[ReportSchemaResponse, A]] = None
  )(implicit
    encoder0: ArgEncoder[Option[String]],
    encoder1: ArgEncoder[SchemaReport]
  ): SelectionBuilder[_root_.caliban.client.Operations.RootMutation, Option[Option[A]]] =
    _root_.caliban.client.SelectionBuilder.Field(
      "reportSchema",
      OptionOf(
        ChoiceOf(
          Map(
            "ReportSchemaError"    -> onReportSchemaError.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a))),
            "ReportSchemaResponse" -> onReportSchemaResponse.fold[FieldBuilder[Option[A]]](NullField)(a =>
              OptionOf(Obj(a))
            )
          )
        )
      ),
      arguments = List(
        Argument("coreSchema", coreSchema, "String")(encoder0),
        Argument("report", report, "SchemaReport!")(encoder1)
      )
    )
}
