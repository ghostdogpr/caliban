package caliban.gateway.internal.execution

import caliban.{ PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.planning.OperationPlan.RequiredSelection
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.StringValue

import scala.collection.mutable

/**
 * A prepared traversal for typename translation, alias restoration and error paths.
 * Values combine selections sharing a response name; paths follow the first selection, as field lookup does.
 */
private[gateway] final class ResponseProjection private (
  fields: java.util.HashMap[String, ResponseProjection.Entry],
  typeNames: Map[String, String],
  typenameCount: Int
) {
  def apply(value: ResponseValue): ResponseValue = project(value, restoresNames)

  def path(value: List[PathValue]): List[PathValue] = value match {
    case PathValue.Key(name) :: tail    =>
      val entry = fields.get(name)
      if ((entry eq null) || (entry.path eq null)) value else PathValue.Key(entry.clientName) :: entry.path.path(tail)
    case PathValue.Index(index) :: tail => PathValue.Index(index) :: path(tail)
    case _                              => value
  }

  private val traversesChildren = {
    val entries = fields.values().iterator()
    var found   = false
    while (!found && entries.hasNext) found = entries.next().value.active
    found
  }

  private val active: Boolean = traversesChildren || typenameCount > 0

  private val restoresNames: Boolean = {
    val entries = fields.entrySet().iterator()
    var found   = false
    while (!found && entries.hasNext) {
      val entry = entries.next()
      found = entry.getKey != entry.getValue.clientName || entry.getValue.value.restoresNames
    }
    found
  }

  private def project(value: ResponseValue, restore: Boolean): ResponseValue =
    if (!active && !restore) value
    else
      value match {
        case StringValue(name) if typenameCount > 0 =>
          var translated = name
          var i          = 0
          while (i < typenameCount) {
            translated = typeNames.getOrElse(translated, translated)
            i += 1
          }
          StringValue(translated)
        case _                                      => projectChildren(value, restore)
      }

  private def projectChildren(value: ResponseValue, restore: Boolean): ResponseValue =
    if (!traversesChildren && !restore) value
    else
      value match {
        case ObjectValue(values) if restore =>
          val restored = mutable.LinkedHashMap.empty[String, ResponseValue]
          values.foreach { case (name, nested) =>
            val entry       = fields.get(name)
            val clientName  = if (entry eq null) name else entry.clientName
            val clientValue = if (entry eq null) nested else entry.value.project(nested, entry.path ne null)
            restored.update(
              clientName,
              restored.get(clientName).fold(clientValue)(ResponseMerge.mergeRootValue(_, clientValue))
            )
          }
          ObjectValue(restored.toList)
        case ObjectValue(values)            =>
          ObjectValue(values.map { field =>
            val entry = fields.get(field._1)
            if ((entry eq null) || !entry.value.active) field else field._1 -> entry.value.project(field._2, false)
          })
        case ListValue(values)              => ListValue(values.map(projectChildren(_, restore)))
        case other                          => other
      }

}

private[gateway] object ResponseProjection {
  def compile(
    client: List[Field],
    executable: List[Field],
    required: List[RequiredSelection],
    typeNames: Map[String, String]
  ): ResponseProjection = {
    def build(
      client: List[Field],
      executable: List[Field],
      required: List[RequiredSelection],
      typenameCount: Int = 0
    ): ResponseProjection = if (client.isEmpty && executable.isEmpty && required.isEmpty) {
      if (typenameCount == 0) identity
      else new ResponseProjection(emptyFields, typeNames, typenameCount)
    } else {
      val paired       = executable.zip(client).groupBy(_._1.aliasedName)
      val requirements = required.groupBy(_.responseName)
      val fields       = new java.util.HashMap[String, Entry]
      (paired.keySet ++ requirements.keySet).foreach { name =>
        val matches         = paired.getOrElse(name, Nil)
        val selections      = requirements.getOrElse(name, Nil)
        val childClient     = matches.flatMap(_._2.fields)
        val childExecutable = matches.flatMap(_._1.fields)
        val count           =
          if (typeNames.isEmpty) 0
          else matches.count(_._1.name == "__typename") + selections.count(_.field == "__typename")
        val child           =
          build(childClient, childExecutable, selections.flatMap(_.children), count)
        val path            = matches match {
          // Aliases shared by fragments combine for values, but errors retain first-match field lookup.
          case (source, target) :: _ :: _ => build(target.fields, source.fields, Nil)
          case Nil                        => null // Correlation selections have no client path or alias restoration.
          case _                          => child
        }
        fields.put(name, Entry(matches.headOption.fold(name)(_._2.aliasedName), child, path))
      }
      new ResponseProjection(fields, typeNames, typenameCount)
    }

    build(client, executable, required)
  }

  private final case class Entry(clientName: String, value: ResponseProjection, path: ResponseProjection)
  private val emptyFields = new java.util.HashMap[String, Entry]
  private val identity    = new ResponseProjection(emptyFields, Map.empty, 0)
}
