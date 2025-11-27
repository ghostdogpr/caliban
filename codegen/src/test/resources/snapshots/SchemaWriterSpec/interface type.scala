import caliban.schema.Annotations._

object Types {

  final case class Admin(
    id: java.util.UUID,
    @GQLDescription("firstName")
    firstName: String,
    lastName: String
  ) extends Person
  final case class Customer(id: java.util.UUID, firstName: String, lastName: String, email: String) extends Person

  @GQLInterface
  @GQLDescription("""person
Admin or Customer""")
  sealed trait Person extends scala.Product with scala.Serializable {
    def id: java.util.UUID
    def firstName: String
    def lastName: String
  }

}
