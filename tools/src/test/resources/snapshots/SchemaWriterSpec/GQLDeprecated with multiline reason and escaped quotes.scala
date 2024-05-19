import caliban.schema.Annotations._

object Types {

  final case class ExampleType(
    @GQLDeprecated("""This field is deprecated for the following reasons:
1. \"Outdated data model\": The field relies on an outdated data model.
2. \"Performance issues\": Queries using this field have performance issues.
Please use `newField` instead.""")
    oldField: scala.Option[String],
    newField: scala.Option[String]
  )

}
