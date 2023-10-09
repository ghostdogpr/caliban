package caliban.gateway

import caliban._
import caliban.introspection.adt.{ __Schema, Extend, TypeVisitor }
import zio.prelude.NonEmptyList
import zio.{ Chunk, RIO, ZIO }

case class SuperGraph[-R](
  protected val subGraphs: List[SubGraph[R]],
  protected val transformers: Chunk[Map[String, __Schema] => TypeVisitor] = Chunk.empty
) { self =>
  def compose[R1](subGraph: SubGraph[R1]): SuperGraph[R with R1] =
    new SuperGraph[R with R1](subGraph :: self.subGraphs, transformers) {}

  def transform(transformer: TypeVisitor): SuperGraph[R] = {
    val t = (_: Map[String, __Schema]) => transformer
    new SuperGraph(self.subGraphs, self.transformers :+ t)
  }

  private def transformWith(makeTransformer: Map[String, __Schema] => TypeVisitor): SuperGraph[R] =
    new SuperGraph(self.subGraphs, self.transformers :+ makeTransformer)

  def extend(
    sourceGraph: SubGraph[Nothing],
    sourceFieldName: String,
    targetTypeName: String,
    targetFieldName: String,
    argumentMappings: Map[String, InputValue => (String, InputValue)],
    filterBatchResults: Option[(ResponseValue.ObjectValue, ResponseValue.ObjectValue) => Boolean] = None
  ): SuperGraph[R] =
    transformWith(
      _.get(sourceGraph.name)
        .fold(TypeVisitor.empty)(schema =>
          schema.queryType.allFields.find(_.name == sourceFieldName) orElse
            schema.mutationType.flatMap(_.allFields.find(_.name == sourceFieldName)) orElse
            schema.subscriptionType.flatMap(_.allFields.find(_.name == sourceFieldName)) match {
            case Some(fieldDefinition) =>
              TypeVisitor.fields.addWith(t =>
                if (t.name.contains(targetTypeName))
                  List(
                    fieldDefinition.copy(
                      name = targetFieldName,
                      args = Nil,
                      extend = Some(Extend(sourceGraph.name, sourceFieldName, argumentMappings, filterBatchResults))
                    )
                  )
                else Nil
              )
            case None                  => TypeVisitor.empty
          }
        )
    )

  def build: RIO[R, GraphQL[R]] =
    for {
      subGraphs   <- ZIO.foreachPar(self.subGraphs)(_.build)
      nel         <- ZIO
                       .succeed(NonEmptyList.fromIterableOption(subGraphs))
                       .someOrFail(new Throwable("At least one subgraph must be defined"))
      subGraphsMap = subGraphs.map(g => g.name -> g.schema).toMap
    } yield SuperGraphExecutor(nel, transformers.map(_(subGraphsMap)))
}

object SuperGraph {
  val empty: SuperGraph[Any] = new SuperGraph[Any](Nil)

  def compose[R](subGraphs: List[SubGraph[R]]): SuperGraph[R] = new SuperGraph[R](subGraphs)
}
