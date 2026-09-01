package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLResponse, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.composition.SchemaMapping
import caliban.gateway.internal.planning.OperationPlan.RequiredSelection
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }

import scala.collection.immutable.ListMap
import scala.collection.mutable

/**
 * Applies a composed schema mapping to fetched values and injected lookup selections.
 */
private[gateway] final class ResponseMapping(mapping: SchemaMapping) {
  import mapping._

  def requiredSelectionToSource(parentType: String, selection: RequiredSelection): RequiredSelection = {
    val sourceParent = sourceType(parentType)
    val sourceName   = sourceField(parentType, selection.field)
    val childType    = sourceFieldDefinition(sourceParent, sourceName).flatMap(_._type.innerType.name).getOrElse("")
    RequiredSelection(
      sourceName,
      selection.responseName,
      selection.children.map(requiredSelectionToSource(clientType(childType), _))
    )
  }

  private[internal] def rootResponseMapper(
    fields: List[Field]
  ): GraphQLResponse[CalibanError] => GraphQLResponse[CalibanError] =
    if (renamesNothing) identity
    else {
      val mapData = objectResponseMapper(fields)
      response => response.copy(data = mapData(response.data))
    }

  private[internal] def entityFieldsResponseMapper(
    fields: List[Field]
  ): ResponseValue => ResponseValue =
    if (renamesNothing) identityResponse
    else objectResponseMapper(fields)

  private val identityResponse: ResponseValue => ResponseValue = identity

  private val typenameResponseMapper: ResponseValue => ResponseValue = {
    case StringValue(name) => StringValue(clientType(name))
    case other             => other
  }

  private def mapSelectedObject(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue],
    values: List[(String, ResponseValue)]
  ): ObjectValue =
    ObjectValue(values.map { case (name, nested) =>
      val mapper = selected.get(name)
      name -> (if (mapper eq null) nested else mapper(nested))
    })

  private def recursiveSelectedResponseMapper(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue]
  ): ResponseValue => ResponseValue = {
    def map(value: ResponseValue): ResponseValue =
      value match {
        case ObjectValue(values) => mapSelectedObject(selected, values)
        case ListValue(values)   => ListValue(values.map(map))
        case other               => other
      }

    map
  }

  private def objectResponseMapper(fields: List[Field]): ResponseValue => ResponseValue = {
    val selected  = new java.util.HashMap[String, ResponseValue => ResponseValue]
    var remaining = fields
    while (remaining ne Nil) {
      val field = remaining.head
      addResponseMapper(selected, field.aliasedName, fieldResponseMapper(field))
      remaining = remaining.tail
    }
    recursiveSelectedResponseMapper(selected)
  }

  private def fieldResponseMapper(field: Field): ResponseValue => ResponseValue =
    if (field.name == "__typename")
      typenameResponseMapper
    else if (field.fields.nonEmpty) objectResponseMapper(field.fields)
    else identityResponse

  private[internal] def requiredResponseMapper(
    typeName: String,
    selections: List[RequiredSelection]
  ): ResponseValue => ResponseValue =
    if (selections.isEmpty) identityResponse
    else {
      val selected  = new java.util.HashMap[String, ResponseValue => ResponseValue]
      var remaining = selections
      while (remaining ne Nil) {
        val selection = remaining.head
        val mapper    =
          if (selection.field == "__typename")
            typenameResponseMapper
          else {
            val sourceName = sourceField(typeName, selection.field)
            if (selection.children.isEmpty) identityResponse
            else {
              val childName = sourceFieldDefinition(sourceType(typeName), sourceName)
                .flatMap(_._type.innerType.name)
                .map(clientType)
                .getOrElse("")
              requiredResponseMapper(childName, selection.children)
            }
          }
        addResponseMapper(selected, selection.responseName, mapper)
        remaining = remaining.tail
      }

      recursiveSelectedResponseMapper(selected)
    }

  private def addResponseMapper(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue],
    name: String,
    mapper: ResponseValue => ResponseValue
  ): Unit = {
    val existing = selected.get(name)
    selected.put(name, if (existing eq null) mapper else mapper.compose(existing))
  }

}

private[gateway] object ResponseMapping {
  private[internal] final case class ResponseNameMapping(
    clientName: String,
    children: Map[String, ResponseNameMapping]
  )

  def responseNameRestorer(
    clientFields: List[Field],
    executableFields: List[Field]
  ): Option[Map[String, ResponseNameMapping]] = {
    val mappings = responseNameMappings(clientFields, executableFields)

    def isIdentity(values: Map[String, ResponseNameMapping]): Boolean =
      values.forall { case (name, mapping) => mapping.clientName == name && isIdentity(mapping.children) }

    if (isIdentity(mappings)) None else Some(mappings)
  }

  def restoreResponseNames(
    mappings: Map[String, ResponseNameMapping],
    value: ResponseValue
  ): ResponseValue =
    value match {
      case ObjectValue(fields) =>
        val restored = mutable.LinkedHashMap.empty[String, ResponseValue]
        fields.foreach { case (name, nested) =>
          val (clientName, clientValue) = mappings.get(name) match {
            case Some(mapping) => mapping.clientName -> restoreResponseNames(mapping.children, nested)
            case None          => name               -> nested
          }
          restored.update(
            clientName,
            restored.get(clientName).fold(clientValue)(mergeResponseValues(_, clientValue))
          )
        }
        ObjectValue(restored.toList)
      case ListValue(values)   => ListValue(values.map(restoreResponseNames(mappings, _)))
      case other               => other
    }

  private def responseNameMappings(
    clientFields: List[Field],
    executableFields: List[Field]
  ): Map[String, ResponseNameMapping] =
    executableFields
      .zip(clientFields)
      .groupBy(_._1.aliasedName)
      .map { case (responseName, matches) =>
        val executable = matches.iterator.map(_._1).reduce(_.combine(_))
        val client     = matches.iterator.map(_._2).reduce(_.combine(_))
        responseName -> ResponseNameMapping(
          client.aliasedName,
          responseNameMappings(client.fields, executable.fields)
        )
      }

  def restoreResponsePath(
    clientFields: List[Field],
    executableFields: List[Field],
    path: List[PathValue]
  ): List[PathValue] =
    path match {
      case PathValue.Key(name) :: tail    =>
        executableFields.zip(clientFields).find(_._1.aliasedName == name) match {
          case Some((executable, client)) =>
            PathValue.Key(client.aliasedName) :: restoreResponsePath(client.fields, executable.fields, tail)
          case None                       => path
        }
      case PathValue.Index(index) :: tail =>
        PathValue.Index(index) :: restoreResponsePath(clientFields, executableFields, tail)
      case _ :: _                         => path
      case Nil                            => Nil
    }

  private def mergeResponseValues(left: ResponseValue, right: ResponseValue): ResponseValue =
    (left, right) match {
      case (ObjectValue(leftFields), ObjectValue(rightFields))                                    =>
        val merged = rightFields.foldLeft(ListMap(leftFields: _*)) { case (values, (name, nested)) =>
          values.updated(name, values.get(name).fold(nested)(mergeResponseValues(_, nested)))
        }
        ObjectValue(merged.toList)
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          mergeResponseValues(leftValue, rightValue)
        })
      case (value, NullValue)                                                                     => value
      case (_, value)                                                                             => value
    }

}
