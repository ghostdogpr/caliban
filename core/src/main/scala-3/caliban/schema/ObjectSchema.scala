package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.DerivationUtils.*
import magnolia1.TypeInfo

import scala.annotation.threadUnsafe
import scala.reflect.ClassTag

final private class ObjectSchema[R, A](
  _constructorFields: => List[(String, Schema[R, Any], Int)],
  _methodFields: => List[(String, List[Any], Schema[R, ?])],
  info: TypeInfo,
  anns: List[Any],
  paramAnnotations: Map[String, List[Any]],
  enableSemanticNonNull: Boolean
)(using ct: ClassTag[A])
    extends Schema[R, A] {

  @threadUnsafe
  private lazy val fields = {
    val fromConstructor = _constructorFields.view.map { (label, schema, index) =>
      val fieldAnns = paramAnnotations.getOrElse(label, Nil)
      ((getName(fieldAnns, label), fieldAnns, schema), Left(index))
    }
    val fromMethods     = _methodFields.view.map { (methodName, fieldAnns, schema) =>
      ((getName(fieldAnns, methodName), fieldAnns, schema.asInstanceOf[Schema[R, Any]]), Right(methodName))
    }

    (fromConstructor ++ fromMethods).toList
  }

  @threadUnsafe
  private lazy val resolver = {
    val clazz = ct.runtimeClass
    val fs    = fields.map { case ((name, _, schema), idx) =>
      name ->
        idx.fold(
          i => (v: A) => schema.resolve(v.asInstanceOf[Product].productElement(i)),
          methodName => {
            val method = clazz.getMethod(methodName)
            (v: A) => schema.resolve(method.invoke(v))
          }
        )
    }
    ObjectFieldResolver(getName(anns, info), fs)
  }

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _ = resolver // Init the lazy val
    if (isInput) mkInputObject[R](anns, fields.map(_._1), info)(isInput, isSubscription)
    else mkObject[R](anns, fields.map(_._1), info, enableSemanticNonNull)(isInput, isSubscription)
  }

  def resolve(value: A): Step[R] = resolver.resolve(value)
}
