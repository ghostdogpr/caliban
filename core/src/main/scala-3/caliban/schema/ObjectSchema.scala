package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.DerivationUtils.*
import magnolia1.TypeInfo

final private class ObjectSchema[R, A](
  _fields: => List[(String, Schema[R, Any], Int)],
  info: TypeInfo,
  anns: List[Any],
  paramAnnotations: Map[String, List[Any]]
) extends Schema[R, A] {

  private lazy val fields = _fields.map { (label, schema, index) =>
    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
    (getName(fieldAnnotations, label), fieldAnnotations, schema, index)
  }

  private lazy val resolver = {
    def fs = fields.map { (name, _, schema, i) =>
      name -> { (v: A) => schema.resolve(v.asInstanceOf[Product].productElement(i)) }
    }
    ObjectFieldResolver(getName(anns, info), fs)
  }

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _ = resolver // Init the lazy val
    if (isInput) mkInputObject[R](anns, fields, info)(isInput, isSubscription)
    else mkObject[R](anns, fields, info)(isInput, isSubscription)
  }

  def resolve(value: A): Step[R] = resolver.resolve(value)
}
