package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.introspection.adt.Extend
import caliban.{ InputValue, ResponseValue }

sealed trait Resolver
object Resolver {
  case class Extractor(extract: ObjectValue => ResponseValue) extends Resolver
  case class Fetcher(extend: Extend)                          extends Resolver

  case class Field(
    name: String,
    outputName: String,
    fields: List[Field],
    arguments: Map[String, InputValue],
    resolver: Resolver,
    eliminate: Boolean
  )

  object Field {
    def apply(field: caliban.execution.Field): Resolver.Field = {
      val name       = field.definition.fold(identity[String] _)(_.renameInput)(field.name)
      val outputName = field.alias.getOrElse(field.name)
      val extend     = field.definition.flatMap(_.extend)
      val fields     =
        if (extend.flatMap(_.target).isEmpty) field.fields.map(apply)
        else
          List(
            Resolver.Field(
              name,
              outputName,
              field.fields.map(apply),
              Map.empty,
              Extractor(_.get(field.name)),
              eliminate = true
            )
          )
      val arguments  = field.arguments.map { case (k, v) =>
        field.definition.fold(identity[String] _)(_.renameArguments)(k) -> v
      }
      val resolver   = extend match {
        case Some(extend) => Fetcher(extend)
        case None         => Extractor(if (field.isRoot) identity else _.get(field.name))
      }
      Resolver.Field(name, outputName, fields, arguments, resolver, eliminate = false)
    }
  }
}
