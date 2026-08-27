package caliban.gateway

import caliban.{ CalibanError, GraphQLRequest, InputValue, PathValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.Field
import caliban.gateway.internal.composition.{ ComposedGraph, SchemaComposer, SchemaMapping }
import caliban.gateway.internal.execution._
import caliban.gateway.internal.execution.ResponseCompletion.{ BubbleNull, Completed }
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan._
import caliban.parsing.Parser
import caliban.parsing.adt.OperationType
import caliban.schema.Types
import caliban.tools.RemoteSchema
import zio.ZIO
import zio.test._

object ExecutionModelSpec extends ZIOSpecDefault {
  private val queryType  = Types.makeObject(Some("Query"), None, Nil, Nil)
  private val objectType = Types.makeObject(Some("Product"), None, Nil, Nil)
  private val completion = new ResponseCompletion(Nil)
  private val name       = Field("name", Types.string, Some(objectType))

  def spec = suite("Execution model")(
    test("nullable null is a completed value, not a bubbling failure") {
      val data = ObjectValue(List("name" -> NullValue))
      assertTrue(completion.complete(List(name), data, Nil) == Completed(data, Nil))
    },
    test("a non-null root failure explicitly bubbles") {
      val field  = name.copy(fieldType = Types.string.nonNull, parentType = Some(queryType))
      val result = completion.complete(List(field), ObjectValue(List("name" -> NullValue)), Nil)
      assertTrue(
        result.isInstanceOf[BubbleNull],
        result.bubblesNull,
        result.toResponseValue == NullValue,
        result.errors.map(_.path) == List(List(PathValue.Key("name")))
      )
    },
    test("bubbling stops at a nullable object and preserves its siblings") {
      val product =
        Field("product", objectType, Some(queryType), fields = List(name.copy(fieldType = Types.string.nonNull)))
      val sibling = Field("status", Types.string, Some(queryType))
      val data    = ObjectValue(List("product" -> ObjectValue(List("name" -> NullValue)), "status" -> StringValue("ok")))
      val result  = completion.complete(List(product, sibling), data, Nil)
      assertTrue(
        !result.bubblesNull,
        result.toResponseValue == ObjectValue(List("product" -> NullValue, "status" -> StringValue("ok"))),
        result.errors.map(_.path) == List(List(PathValue.Key("product"), PathValue.Key("name")))
      )
    },
    test("a non-null list item nulls the nullable list") {
      val field  = Field("names", Types.string.nonNull.list, Some(queryType))
      val data   = ObjectValue(List("names" -> ListValue(List(StringValue("first"), NullValue))))
      val result = completion.complete(List(field), data, Nil)
      assertTrue(
        !result.bubblesNull,
        result.toResponseValue == ObjectValue(List("names" -> NullValue)),
        result.errors.map(_.path) == List(List(PathValue.Key("names"), PathValue.Index(1)))
      )
    },
    test("an existing source error is not duplicated during null propagation") {
      val field  = name.copy(fieldType = Types.string.nonNull)
      val error  = CalibanError.ExecutionError("source failure", path = List(PathValue.Key("name")))
      val result = completion.complete(List(field), ObjectValue(List("name" -> NullValue)), List(error))
      assertTrue(result == BubbleNull(Nil))
    },
    test("execution artifacts are reused but variable binding gets an independent cache") {
      val field      = Field("product", objectType, Some(queryType), arguments = Map("id" -> InputValue.VariableValue("id")))
      val fetch      = RootFetch(FetchId(0), "products", List(field), List(field))
      val plan       = OperationPlan(OperationType.Query, "Query", List(field), Nil, List(fetch), Nil, Nil, None)
      val prepared   = new PreparedPlan(plan)
      val cache      = prepared.cache
      val completion = prepared.completion
      val bound      = prepared.bind(Map("id" -> StringValue("p1")))
      assertTrue(
        cache eq prepared.cache,
        completion eq prepared.completion,
        prepared.cache ne bound.cache,
        prepared.completion ne bound.completion,
        prepared.operation == plan.operation,
        prepared.render == plan.render,
        bound.cache.roots.isEmpty,
        bound.plan.roots.head.downstream.head.arguments == Map("id" -> StringValue("p1")),
        prepared.plan.roots.head.downstream.head.arguments == Map("id" -> InputValue.VariableValue("id"))
      )
    },
    test("a missing entity executor returns a failure and blocks its dependent fetches") {
      val schema  = "type Query { product: Product } type Product { id: ID! name: String }"
      val rootId  = FetchId(0)
      val fetchId = FetchId(1)
      val path    = List(PathValue.Key("product"))
      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        rootType <- ZIO.fromEither(RemoteSchema.toRootType(document))
        mapping  <- ZIO.fromEither(SchemaMapping.compile("products", rootType, document, federation = false, Nil))
        graph    <- ZIO.fromEither(
                      SchemaComposer.compose(List(PreparedSubgraph("products", rootType, document, false, Nil, mapping)))
                    )
        fetch     = EntityFetch(
                      id = fetchId,
                      root = rootId,
                      source = "products",
                      dependencies = Set(rootId),
                      dependencySource = "products",
                      mergePath = Vector("product"),
                      entityType = "Product",
                      keys = List(RequiredSelection("id", "id")),
                      requirements = Nil,
                      typename = None,
                      lookup = ComposedGraph.EntityLookup(
                        List(ComposedGraph.KeyField("id", Nil)),
                        ComposedGraph.LookupOperation.FederationEntities(None)
                      ),
                      fields = List(name),
                      mayNeedPrerequisiteFetches = false
                    )
        executor  = new EntityExecutor[Any](graph, Map.empty, Map("products" -> new ResponseMapping(mapping)))
        plan      = new PreparedPlan(OperationPlan(OperationType.Query, "Query", Nil, Nil, Nil, List(fetch), Nil, None))
        results  <- executor.execute(
                      List(fetch),
                      Map(rootId -> ObjectValue(List("product" -> ObjectValue(List("id" -> StringValue("p1")))))),
                      Map.empty,
                      GraphQLRequest(),
                      plan.cache
                    )
      } yield assertTrue(
        graph.mapping("products").nonEmpty,
        results.size == 1,
        results.head.patches.isEmpty,
        results.head.errors == List(RemoteError.at(path)),
        results.head.completed == Set(fetchId),
        results.head.blocked == Map(fetchId -> Set(path))
      )
    }
  )
}
