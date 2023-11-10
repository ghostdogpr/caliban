package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.DerivationUtils.*
import caliban.schema.Step.{ MetadataFunctionStep, ObjectStep }
import magnolia1.TypeInfo

final private class ObjectSchema[R, A](
  _fields: => List[(String, Schema[R, Any], Int)],
  info: TypeInfo,
  anns: List[Any],
  paramAnnotations: Map[String, List[Any]]
) extends Schema[R, A] {
  private val name = getName(anns, info)

  private lazy val fields = _fields.map { (label, schema, index) =>
    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
    (getName(fieldAnnotations, label), fieldAnnotations, schema, index)
  }

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (isInput) mkInputObject[R](anns, fields, info)(isInput, isSubscription)
    else mkObject[R](anns, fields, info)(isInput, isSubscription)

  def resolve(value: A): Step[R] = MetadataFunctionStep[R] { f =>
    val fb = Map.newBuilder[String, Step[R]]

    var remaining = fields
    while (!remaining.isEmpty) {
      val (name, _, schema, i) = remaining.head
      if (f.fieldNames.contains(name)) fb += name -> schema.resolve(value.asInstanceOf[Product].productElement(i))
      remaining = remaining.tail
    }

    ObjectStep(name, fb.result())
  }
}
