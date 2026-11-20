package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.DerivationUtils.*
import magnolia1.TypeInfo

import scala.annotation.threadUnsafe

final private class ObjectSchema[R, A](
  _constructorFields: => List[ProductFieldInfo[R]],
  _methodFields: => List[MethodFieldInfo[R, A]],
  info: TypeInfo,
  anns: List[Any],
  paramAnnotations: Map[String, List[Any]],
  enableSemanticNonNull: Boolean
) extends Schema[R, A] {

  @threadUnsafe
  private lazy val fields = {
    val fromConstructor = _constructorFields.view.map { case ProductFieldInfo(label, schema, index) =>
      val fieldAnns = paramAnnotations.getOrElse(label, Nil)
      (
        (getName(fieldAnns, label), fieldAnns, schema),
        (v: A) => schema.resolve(v.asInstanceOf[Product].productElement(index))
      )
    }
    val fromMethods     = _methodFields.view.map { case MethodFieldInfo(methodName, fieldAnns, schema, resolve) =>
      ((getName(fieldAnns, methodName), fieldAnns, schema.asInstanceOf[Schema[R, Any]]), resolve)
    }

    (fromConstructor ++ fromMethods).toList
  }

  @threadUnsafe
  private lazy val resolver =
    ObjectFieldResolver(getName(anns, info), fields.map { case ((name, _, _), resolve) => name -> resolve })

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _ = resolver // Init the lazy val
    if (isInput) mkInputObject[R](anns, fields.map(_._1), info)(isInput, isSubscription)
    else mkObject[R](anns, fields.map(_._1), info, enableSemanticNonNull)(isInput, isSubscription)
  }

  def resolve(value: A): Step[R] = resolver.resolve(value)
}
