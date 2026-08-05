package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.introspection.adt.Extend
import caliban.transformers.Transformer
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
    def apply(field: caliban.execution.Field, transformer: Transformer[Any]): Resolver.Field = {
      val (_, name, arguments) = transformer.translateInput(
        field.parentType.flatMap(_.name).getOrElse(""),
        field.name,
        field.arguments
      )
      val outputName           = field.alias.getOrElse(field.name)
      val extend               = field.definition.flatMap(_.extend)
      val fields               =
        if (extend.flatMap(_.target).isEmpty) field.fields.map(apply(_, transformer))
        else
          List(
            Resolver.Field(
              name,
              outputName,
              field.fields.map(apply(_, transformer)),
              Map.empty,
              Extractor(_.get(name)),
              eliminate = true
            )
          )
      val resolver             = extend match {
        case Some(extend) => Fetcher(extend)
        case None         => Extractor(if (field.isRoot) identity else _.get(name))
      }
      Resolver.Field(name, outputName, fields, arguments, resolver, eliminate = false)
    }
  }
}
