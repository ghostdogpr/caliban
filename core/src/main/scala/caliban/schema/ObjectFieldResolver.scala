package caliban.schema

import caliban.execution.Field
import caliban.schema.Step.{ MetadataFunctionStep, ObjectStep }

import scala.collection.mutable

final private class ObjectFieldResolver[R, A](
  objectName: String,
  fields: Iterable[(String, A => Step[R])]
) {

  private val fieldsMap: java.util.HashMap[String, A => Step[R]] = {
    val map = new java.util.HashMap[String, A => Step[R]]()
    fields.foreach { case (name, resolve) => map.put(name, resolve) }
    map
  }

  def resolve(value: A): Step[R] = MetadataFunctionStep(resolveForField(value, _))

  private def resolveForField(
    value: A,
    field: Field
  ): Step[R] = {
    val fieldsBuilder = new mutable.HashMap[String, Step[R]]()

    var remaining = field.distinctFieldNames
    while (!remaining.isEmpty) {
      val name    = remaining.head
      val resolve = fieldsMap.get(name)
      if (resolve ne null) fieldsBuilder.update(name, resolve(value))
      remaining = remaining.tail
    }
    ObjectStep(objectName, fieldsBuilder)
  }
}
