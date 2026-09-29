package caliban.gateway.internal.execution

import caliban.{ PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.{ groupNonEmpty, TypenameField }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.StringValue

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * A prepared traversal for typename translation, alias restoration and error paths.
 * Values combine selections sharing a response name; paths follow the first selection, as field lookup does.
 */
private[gateway] final class ResponseProjection private (
  fields: java.util.HashMap[String, ResponseProjection.Entry],
  typeNames: Map[String, String]
) {
  def apply(value: ResponseValue): ResponseValue = project(value)

  def path(errorPath: List[PathValue]): List[PathValue] = errorPath match {
    case PathValue.Key(name) :: tail    =>
      val entry = fields.get(name)
      if (entry eq null) errorPath
      else PathValue.Key(entry.clientName) :: entry.path.path(tail)
    case PathValue.Index(index) :: tail => PathValue.Index(index) :: path(tail)
    case _                              => errorPath
  }

  private val translatesTypename = typeNames.nonEmpty

  private val restoresNames: Boolean =
    fields.asScala.exists { case (name, entry) => name != entry.clientName }

  private val traversesChildren = fields.values().asScala.exists(_.value.active)

  private val active: Boolean = traversesChildren || translatesTypename || restoresNames

  private def project(value: ResponseValue): ResponseValue =
    if (!active) value
    else
      value match {
        case StringValue(name) if translatesTypename => StringValue(typeNames.getOrElse(name, name))
        case _                                       => projectChildren(value)
      }

  private def projectChildren(value: ResponseValue): ResponseValue =
    value match {
      case ObjectValue(values) if restoresNames =>
        val restored = mutable.LinkedHashMap.empty[String, ResponseValue]
        values.foreach { case field @ (name, nested) =>
          val entry       = fields.get(name)
          val clientName  = if (entry eq null) name else entry.clientName
          val clientValue = if (entry eq null) nested else entry.value.project(nested)
          restored.get(clientName) match {
            case None                                                           => restored.update(clientName, clientValue)
            // A repeated upstream key keeps its first value, as completion reads it.
            case Some(previous) if values.find(_._1 == name).exists(_ eq field) =>
              restored.update(clientName, ResponseMerge.mergeRootValue(previous, clientValue))
            case _                                                              =>
          }
        }
        ObjectValue(restored.toList)
      case ObjectValue(values)                  =>
        ObjectValue(values.map { field =>
          val entry = fields.get(field._1)
          if ((entry eq null) || !entry.value.active) field else field._1 -> entry.value.project(field._2)
        })
      case ListValue(values)                    => ListValue(values.map(projectChildren))
      case other                                => other
    }

}

private[gateway] object ResponseProjection {
  def compile(client: List[Field], executable: List[Field], typeNames: Map[String, String]): ResponseProjection = {
    def build(
      client: List[Field],
      executable: List[Field],
      translations: Map[String, String] = Map.empty
    ): ResponseProjection =
      if (client.isEmpty && executable.isEmpty && translations.isEmpty) Identity
      else {
        val fields = new java.util.HashMap[String, Entry]
        groupNonEmpty(executable.zip(client))(_._1.aliasedName).foreach {
          case (name, matches @ ::((source, target), rest)) =>
            val translations = if (matches.exists(_._1.name == TypenameField)) typeNames else Map.empty[String, String]
            val child        = build(matches.flatMap(_._2.fields), matches.flatMap(_._1.fields), translations)
            // Aliases shared by fragments combine for values, but errors retain first-match field lookup.
            val path         = if (rest.isEmpty) child else build(target.fields, source.fields)
            fields.put(name, Entry(target.aliasedName, child, path))
        }
        new ResponseProjection(fields, translations)
      }

    build(client, executable)
  }

  private final case class Entry(clientName: String, value: ResponseProjection, path: ResponseProjection)
  private val Identity = new ResponseProjection(new java.util.HashMap[String, Entry], Map.empty)
}
