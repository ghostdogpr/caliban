import caliban.schema.Annotations._

object Types {

  final case class Captain(
    @GQLDescription("ship")
    shipName: String
  ) extends Role
      with Role2
  final case class Pilot(shipName: String)   extends Role with Role2
  final case class Stewart(shipName: String) extends Role2

  @GQLDescription("""role
Captain or Pilot""")
  sealed trait Role extends scala.Product with scala.Serializable
  @GQLDescription("""role2
Captain or Pilot or Stewart""")
  sealed trait Role2 extends scala.Product with scala.Serializable

}
