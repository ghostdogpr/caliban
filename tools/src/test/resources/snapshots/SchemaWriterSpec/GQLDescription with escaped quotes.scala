import caliban.schema.Annotations._

object Types {

  final case class Captain(
    @GQLDescription("foo \"quotes\" bar")
    shipName: String
  )

}
