package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.introspection.adt.Extend
import caliban.{ InputValue, ResponseValue }

sealed trait Resolver
object Resolver {
  case class Extractor(extract: ObjectValue => ResponseValue, fields: List[Field])            extends Resolver
  case class Fetcher(extend: Extend, fields: List[Field], arguments: Map[String, InputValue]) extends Resolver

  case class Field(name: String, outputName: String, resolver: Resolver, eliminate: Boolean) {
    lazy val fields: List[Field] = resolver match {
      case e: Extractor => e.fields
      case f: Fetcher   => f.fields
    }
  }

  object Field {
    def apply(field: caliban.execution.Field): Resolver.Field = {
      val name       = field.definition.fold(identity[String] _)(_.renameInput)(field.name)
      val outputName = field.alias.getOrElse(field.name)
      val fields     = field.definition.flatMap(_.extend) match {
        case Some(extend) =>
          Fetcher(
            extend = extend,
            fields =
              if (extend.target.isEmpty) field.fields.map(apply)
              else
                List(
                  Resolver
                    .Field(name, outputName, Extractor(_.get(field.name), field.fields.map(apply)), eliminate = true)
                ),
            arguments = field.arguments.map { case (k, v) =>
              field.definition.fold(identity[String] _)(_.renameArguments)(k) -> v
            }
          )
        case None         =>
          Extractor(if (field.isRoot) identity else _.get(field.name), field.fields.map(apply))
      }
      Resolver.Field(name, outputName, fields, eliminate = false)
    }
  }
}
