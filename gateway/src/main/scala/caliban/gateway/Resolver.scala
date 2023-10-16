package caliban.gateway

import caliban.ResponseValue.ObjectValue
import caliban.{ InputValue, ResponseValue }

sealed trait Resolver
object Resolver {
  case class Extractor(extract: ObjectValue => ResponseValue, fields: List[Field] = Nil) extends Resolver
  case class Fetcher(
    subGraph: String,
    sourceFieldName: String,
    fields: List[Field],
    arguments: Map[String, InputValue],
    argumentMappings: Map[String, InputValue => (String, InputValue)] = Map.empty,
    filterBatchResults: Option[(ResponseValue.ObjectValue, ResponseValue.ObjectValue) => Boolean]
  ) extends Resolver

  case class Field(name: String, resolver: Resolver, outputName: String)

  object Field {
    def apply(field: caliban.execution.Field): Resolver.Field =
      Resolver.Field(
        field.definition.fold(identity[String] _)(_.renameInput)(field.name),
        field.definition.flatMap(_.extend) match {
          case Some(extend) =>
            Fetcher(
              extend.sourceGraph,
              sourceFieldName = extend.sourceFieldName,
              fields = field.fields.map(apply),
              arguments = field.arguments.map { case (k, v) =>
                field.definition.fold(identity[String] _)(_.renameArguments)(k) -> v
              },
              argumentMappings = extend.argumentMappings,
              filterBatchResults = extend.filterBatchResults
            )
          case None         =>
            Extractor(if (field.isRoot) identity else _.get(field.name), field.fields.map(apply))
        },
        field.alias.getOrElse(field.name)
      )
  }
}
