package caliban.schema

import caliban.introspection.adt.__Field
import caliban.parsing.adt.Directive

transparent trait SchemaVersionSpecific extends GenericSchema[Any] {

  /**
   * Scala 3 variant of the `obj` method which improves UX for creating custom object schemas.
   *
   * {{{
   *  case class Author(id: String, firstName: String, lastName: String)
   *
   *  given Schema[Any, Book] = Schema.custom("Author"))(
   *    field("id")(_.id),
   *    field("fullName")(author => s"${author.firstName} ${author.lastName}"),
   *  )
   * }}}
   *
   * @see [[caliban.schema.GenericSchema.obj]]
   */
  def custom[R1, V](
    name: String,
    description: Option[String] = None,
    directives: List[Directive] = Nil
  )(
    fields: FieldAttributes ?=> (__Field, V => Step[R1])*
  ): Schema[R1, V] =
    obj(name, description, directives) { case given FieldAttributes =>
      fields.toList.map(identity)
    }

}
