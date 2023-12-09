package caliban.schema

import caliban.schema.Step.{ NullStep, ObjectStep }

import scala.collection.compat._
import scala.collection.mutable

final private class ObjectFieldResolver[R, A] private (
  name: String,
  fields: mutable.HashMap[String, A => Step[R]]
) {
  def resolve(value: A): Step[R] = ObjectStep(
    name,
    fields.get(_) match {
      case Some(f) => f(value)
      case None    => NullStep
    }
  )
}

private object ObjectFieldResolver {
  def apply[R, A](objectName: String, fields: Iterable[(String, A => Step[R])]): ObjectFieldResolver[R, A] =
    // NOTE: mutable.HashMap is about twice as fast than immutable.HashMap for .get
    new ObjectFieldResolver(objectName, mutable.HashMap.from(fields))
}
