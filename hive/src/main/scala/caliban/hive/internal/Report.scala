package caliban.hive.internal

import caliban.ResponseValue
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, IntValue, NullValue, StringValue }
import caliban.hive.HiveUsage
import zio.Chunk

/**
 * One reported execution, with everything Hive needs to file it.
 *
 * @param duration in nanoseconds; Hive's format has none for subscriptions
 */
private[hive] final case class Record(
  key: String,
  body: String,
  name: Option[String],
  coordinates: Set[String],
  subscription: Boolean,
  timestamp: Long,
  duration: Long,
  errors: Int,
  client: Option[HiveUsage.ClientInfo]
)

private[hive] object Record {

  /**
   * The record of an execution, or none for an operation that touched no schema coordinate (pure introspection).
   */
  def of(operation: HiveUsage.Operation): Option[Record] = {
    val coordinates = Operations.coordinates(operation.request.field)
    if (coordinates.isEmpty) None
    else {
      val body = Operations.normalize(operation.document)
      val name = Operations.name(operation.document, operation.operationName)
      Some(
        Record(
          key = Operations.key(body, name, coordinates),
          body = body,
          name = name,
          coordinates = coordinates,
          subscription = operation.subscription,
          timestamp = operation.timestamp,
          duration = operation.duration.toNanos,
          errors = operation.errors,
          client = operation.client
        )
      )
    }
  }
}

/**
 * Hive's usage report, format version 2: one map entry per distinct operation, one item per execution.
 */
private[hive] object Report {

  def encode(records: Chunk[Record]): String = {
    val map                         = records.groupBy(_.key).toList.sortBy(_._1).flatMap { case (key, entries) =>
      entries.headOption.map { record =>
        key -> obj(
          "operation"     -> StringValue(record.body),
          "operationName" -> record.name.fold[ResponseValue](NullValue)(StringValue(_)),
          "fields"        -> ListValue(record.coordinates.toList.sorted.map(StringValue(_)))
        )
      }
    }
    val (subscriptions, operations) = records.partition(_.subscription)
    obj(
      "size"                   -> IntValue(records.size),
      "map"                    -> ObjectValue(map),
      "operations"             -> ListValue(operations.toList.map { record =>
        ObjectValue(
          List(
            "operationMapKey" -> StringValue(record.key),
            "timestamp"       -> IntValue(record.timestamp),
            "execution"       -> obj(
              "ok"          -> BooleanValue(record.errors == 0),
              "duration"    -> IntValue(record.duration),
              "errorsTotal" -> IntValue(record.errors)
            )
          ) ++ metadata(record)
        )
      }),
      "subscriptionOperations" -> ListValue(subscriptions.toList.map { record =>
        ObjectValue(
          List(
            "operationMapKey" -> StringValue(record.key),
            "timestamp"       -> IntValue(record.timestamp)
          ) ++ metadata(record)
        )
      })
    ).toString
  }

  /** Left out without client information, as Hive's JS client does. */
  private def metadata(record: Record): List[(String, ResponseValue)] =
    record.client.toList.map { client =>
      "metadata" -> obj("client" -> obj("name" -> StringValue(client.name), "version" -> StringValue(client.version)))
    }

  private def obj(fields: (String, ResponseValue)*): ObjectValue = ObjectValue(fields.toList)
}
