package caliban.gateway

import caliban._
import caliban.introspection.adt.{ __DeprecatedArgs, __Schema, __TypeKind, Extend, TypeVisitor }
import caliban.transformers.Transformer
import sttp.client4.Backend
import zio.{ Chunk, RIO, Task, ZIO }

case class SuperGraph[-R](
  protected val subGraphs: List[SubGraph[R]],
  protected val extensions: Chunk[SuperGraph.Extension] = Chunk.empty,
  protected val transformer: Transformer[Any] = Transformer.empty[Any]
) { self =>
  def compose[R1](subGraph: SubGraph[R1]): SuperGraph[R with R1] =
    new SuperGraph[R with R1](subGraph :: self.subGraphs, extensions, transformer) {}

  def transform(next: Transformer[Any]): SuperGraph[R] =
    new SuperGraph(self.subGraphs, self.extensions, self.transformer |+| next)

  def extend(
    sourceGraph: SubGraph[Nothing],
    sourceFieldName: String,
    targetTypeName: String,
    targetFieldName: String,
    argumentMappings: Map[String, InputValue => (String, InputValue)],
    filterBatchResults: Option[(ResponseValue.ObjectValue, ResponseValue.ObjectValue) => Boolean] = None,
    additionalFields: List[String] = Nil
  ): SuperGraph[R] =
    new SuperGraph(
      self.subGraphs,
      self.extensions :+ SuperGraph.Extension(
        sourceGraph.name,
        sourceFieldName,
        targetTypeName,
        targetFieldName,
        argumentMappings,
        filterBatchResults,
        additionalFields
      ),
      self.transformer
    )

  def build: RIO[R, GraphQL[R]] =
    for {
      subGraphs         <- ZIO.foreachPar(self.subGraphs)(_.build)
      nonEmptySubGraphs <- ZIO
                             .fromOption(if (subGraphs.nonEmpty) Some(subGraphs) else None)
                             .orElseFail(new Throwable("At least one subgraph must be defined"))
      subGraphsMap       = subGraphs.map(g => g.name -> g.schema).toMap
      subGraphVisitors   = Chunk.fromIterable(subGraphs).flatMap(_.visitors)
      allVisitors        = subGraphVisitors ++ extensions.map(_.visitor(subGraphsMap))
    } yield SuperGraphExecutor(nonEmptySubGraphs, allVisitors, transformer)
}

object SuperGraph {
  final case class Extension private[gateway] (
    sourceGraphName: String,
    sourceFieldName: String,
    targetTypeName: String,
    targetFieldName: String,
    argumentMappings: Map[String, InputValue => (String, InputValue)],
    filterBatchResults: Option[(ResponseValue.ObjectValue, ResponseValue.ObjectValue) => Boolean],
    additionalFields: List[String]
  ) {
    private[gateway] def visitor(subGraphs: Map[String, __Schema]): TypeVisitor =
      subGraphs
        .get(sourceGraphName)
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
                      args = _ => Nil,
                      extend = Some(
                        Extend(
                          sourceGraphName,
                          sourceFieldName,
                          argumentMappings,
                          filterBatchResults,
                          additionalFields
                        )
                      )
                    )
                  )
                else Nil
              )
            case None                  => TypeVisitor.empty
          }
        )
  }

  val empty: SuperGraph[Any] = new SuperGraph[Any](Nil)

  def compose[R](subGraphs: List[SubGraph[R]]): SuperGraph[R] = new SuperGraph[R](subGraphs)

  def fromSchema(schema: __Schema): SuperGraph[Backend[Task]] = {
    val subgraphs = schema.types.collectFirst {
      case t if t.kind == __TypeKind.ENUM && t.name.contains("join__Graph") =>
        val entries = t.enumValues(__DeprecatedArgs()).getOrElse(Nil)
        entries.flatMap { entry =>
          for {
            value <- entry.directives.flatMap(_.find(_.name == "join__graph"))
            url   <- value.arguments.get("url").collectFirst { case Value.StringValue(url) => url }
            name  <- value.arguments.get("name").collectFirst { case Value.StringValue(name) => name }
          } yield SubGraph.federated(name, url)
        }
    }.getOrElse(Nil)

    compose(subgraphs)
  }
}
