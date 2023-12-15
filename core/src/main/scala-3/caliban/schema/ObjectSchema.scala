package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.DerivationUtils.*
import magnolia1.TypeInfo

import scala.annotation.threadUnsafe
import scala.reflect.ClassTag

final private class ObjectSchema[R, A](
  _constructorFields: => List[(String, Schema[R, Any], Int)],
  _methodFields: => List[(String, Schema[R, ?])],
  info: TypeInfo,
  anns: List[Any],
  paramAnnotations: Map[String, List[Any]]
)(using ct: ClassTag[A])
    extends Schema[R, A] {

  @threadUnsafe
  private lazy val constructorFields = _constructorFields.map { (label, schema, index) =>
    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
    (getName(fieldAnnotations, label), fieldAnnotations, schema, index)
  }

  @threadUnsafe
  private lazy val methodFields = _methodFields.map { (label, schema) =>
    val fieldAnnotations = paramAnnotations.getOrElse(label, Nil)
    (getName(fieldAnnotations, label), fieldAnnotations, schema.asInstanceOf[Schema[R, Any]])
  }

  @threadUnsafe
  private lazy val resolver = {
    val clazz           = ct.runtimeClass
    def fromConstructor = constructorFields.map { (name, _, schema, i) =>
      name -> { (v: A) => schema.resolve(v.asInstanceOf[Product].productElement(i)) }
    }
    def fromMethods     = methodFields.map { (name, _, schema) =>
      name -> {
        val method = clazz.getMethod(name)
        (v: A) => schema.resolve(method.invoke(v))
      }
    }
    ObjectFieldResolver(getName(anns, info), fromMethods ::: fromConstructor)
  }

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _              = resolver // Init the lazy val
    val combinedFields = constructorFields.map(t => (t._1, t._2, t._3)) ::: methodFields
    if (isInput) mkInputObject[R](anns, combinedFields, info)(isInput, isSubscription)
    else mkObject[R](anns, combinedFields, info)(isInput, isSubscription)
  }

  def resolve(value: A): Step[R] = resolver.resolve(value)
}
