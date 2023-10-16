package caliban.gateway

import caliban.CalibanError.ExecutionError
import caliban.Value.NullValue
import caliban.execution.Field
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.parsing.adt.OperationType
import caliban.schema.Types
import caliban.{ InputValue, ResponseValue }
import zio.query.DataSource
import zio.{ Chunk, ZIO }

object FetchDataSource {
  private[caliban] case class FetchRequest[-R](
    subGraph: SubGraphExecutor[R],
    sourceFieldName: String,
    operationType: OperationType,
    fields: List[Field],
    arguments: Map[String, InputValue],
    batchEnabled: Boolean
  ) extends zio.query.Request[ExecutionError, ResponseValue]

  private[caliban] def apply[R]: DataSource[R, FetchRequest[R]] =
    DataSource.fromFunctionBatchedZIO[R, ExecutionError, FetchRequest[R], ResponseValue]("RemoteDataSource") {
      requests =>
        val requestsMap = requests.groupBy(_.subGraph).flatMap { case (subGraph, requests) =>
          val subGraphRequest: (SubGraphExecutor[R], OperationType, (Field, Map[FetchRequest[R], String])) =
            (
              subGraph,
              requests.head.operationType,
              requests
                .groupBy(_.sourceFieldName)
                .foldLeft(Field("", Types.string, None) -> Map.empty[FetchRequest[R], String]) {
                  case ((field, rootFieldMap), (sourceFieldName, requests)) =>
                    val (mergedFields, updatedRootFieldMap) = {
                      val batchEnabled           = requests.forall(_.batchEnabled)
                      val fields                 = requests
                        .map(request =>
                          request -> Field(
                            sourceFieldName,
                            Types.string,
                            None,
                            fields = request.fields,
                            arguments = request.arguments
                          )
                        )
                      val (firstReq, firstField) = fields.head
                      fields.tail.foldLeft((List(firstField), Map(firstReq -> sourceFieldName))) {
                        case ((fields, rootFieldMap), (request, field)) =>
                          val (merged, res) = {
                            if (batchEnabled)
                              fields
                                .foldLeft((false, List.empty[Field])) { case ((merged, res), f) =>
                                  if (merged) (merged, f :: res)
                                  else
                                    combineFieldArguments(field, f) // TODO also combine subfields
                                      .fold((false, f :: res))(merged =>
                                        (
                                          true,
                                          merged
                                            .copy(alias = f.alias, fields = (f.fields ++ field.fields).distinct) :: res
                                        )
                                      )
                                }
                            else (false, fields)
                          }
                          if (merged) (res, rootFieldMap.updated(request, field.name))
                          else {
                            val alias = s"${field.name}${res.size}"
                            (field.copy(alias = Some(alias)) :: res, rootFieldMap.updated(request, alias))
                          }
                      }
                    }
                    field.copy(fields = mergedFields ++ field.fields) -> (rootFieldMap ++ updatedRootFieldMap)
                }
            )

          requests.map(_ -> subGraphRequest)
        }

        ZIO
          .foreachPar(Chunk.fromIterable(requestsMap.values).distinct) {
            case key @ (subGraph, operationType, (field, _)) =>
              subGraph.run(field, operationType).map(key -> _)
          }
          .map(_.toMap)
          .map(results =>
            requests.map(req =>
              requestsMap
                .get(req)
                .flatMap { case key @ (_, _, (_, rootFieldMap)) =>
                  for {
                    result <- results.get(key)
                    obj    <- result.asObjectValue
                    res    <- rootFieldMap.get(req).map(obj.get)
                  } yield res
                }
                .getOrElse(NullValue)
            )
          )
    }

  private def combineFieldArguments(
    f1: Field,
    f2: Field
  ): Option[Field] =
    mergeInputValueMaps(f1.arguments, f2.arguments).flatMap { mergedArguments =>
      import zio.prelude._
      (f1.fields zip f2.fields).forEach { case (f1, f2) => combineFieldArguments(f1, f2) }.map { mergedFields =>
        f1.copy(arguments = mergedArguments, fields = mergedFields)
      }
    }

  private def mergeInputValueMaps(
    m1: Map[String, InputValue],
    m2: Map[String, InputValue]
  ): Option[Map[String, InputValue]] = {
    val keys = m1.keySet ++ m2.keySet
    keys.foldLeft(Option(Map.empty[String, InputValue])) {
      case (None, _)        => None
      case (Some(acc), key) =>
        (m1.get(key), m2.get(key)) match {
          case (Some(i1), Some(i2)) =>
            (i1, i2) match {
              case (InputValue.ListValue(v1), InputValue.ListValue(v2))     =>
                Some(acc.updated(key, InputValue.ListValue((v1 ++ v2).distinct)))
              case (InputValue.ObjectValue(v1), InputValue.ObjectValue(v2)) =>
                mergeInputValueMaps(v1, v2).map(merged => acc.updated(key, InputValue.ObjectValue(merged)))
              case _                                                        =>
                if (i1 == i2) Some(acc.updated(key, i1))
                else None
            }
          case _                    => None
        }
    }
  }
}