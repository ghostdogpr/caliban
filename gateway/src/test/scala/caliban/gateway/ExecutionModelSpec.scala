package caliban.gateway

import caliban.{ InputValue, PathValue, ResponseValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.Field
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution._
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan._
import caliban.parsing.adt.OperationType
import caliban.schema.Types
import zio.{ IO, ZIO }
import zio.test._

object ExecutionModelSpec extends ZIOSpecDefault {
  private val queryType  = Types.makeObject(Some("Query"), None, Nil, Nil)
  private val objectType = Types.makeObject(Some("Product"), None, Nil, Nil)
  private val completion = new ResponseCompletion(Nil)
  private val name       = Field("name", Types.string, Some(objectType))

  private def projectionField(name: String, alias: String, children: List[Field] = Nil): Field =
    Field(name, Types.string, None, alias = Some(alias), fields = children)

  private def composeSingle(name: String, schema: String): IO[Any, ComposedGraph] =
    GatewayTestSupport
      .parseSdl(schema)
      .flatMap(document =>
        ZIO.fromEither(GatewayTestSupport.composeDocuments(List(name -> document), federation = false))
      )

  def spec = suite("Execution model")(
    test("translates typenames and aliases through lists while preserving scalar payloads and correlation fields") {
      val payload    = ObjectValue(List("__typename" -> StringValue("Source")))
      val client     = List(
        projectionField(
          "items",
          "items",
          List(projectionField("__typename", "kind"), projectionField("payload", "payload"))
        )
      )
      val executable = List(
        projectionField(
          "items",
          "_items",
          List(projectionField("__typename", "_kind"), projectionField("payload", "payload"))
        )
      )
      val required   = List(RequiredSelection("__typename", "_correlationType"))
      val projection = ResponseProjection.compile(client, executable, required, Map("Source" -> "Client"))
      val value      = ObjectValue(
        List(
          "_items"           -> ListValue(
            List(ObjectValue(List("_kind" -> StringValue("Source"), "payload" -> payload)), NullValue)
          ),
          "_correlationType" -> StringValue("Source")
        )
      )
      assertTrue(
        projection(value) == ObjectValue(
          List(
            "items"            -> ListValue(
              List(ObjectValue(List("kind" -> StringValue("Client"), "payload" -> payload)), NullValue)
            ),
            "_correlationType" -> StringValue("Client")
          )
        ),
        projection.path(List(PathValue.Key("_items"), PathValue.Index(0), PathValue.Key("_kind"))) ==
          List(PathValue.Key("items"), PathValue.Index(0), PathValue.Key("kind")),
        projection.path(List(PathValue.Key("unknown"), PathValue.Key("_kind"))) ==
          List(PathValue.Key("unknown"), PathValue.Key("_kind"))
      )
    },
    test("merges aliases landing on the same client field without losing siblings or null fallback") {
      val client     =
        List(
          projectionField("item", "item", List(projectionField("name", "name"))),
          projectionField("item", "item", List(projectionField("id", "id")))
        )
      val executable = List(client.head.copy(alias = Some("_first")), client(1).copy(alias = Some("_second")))
      val projection = ResponseProjection.compile(client, executable, Nil, Map.empty)
      val first      = ObjectValue(List("name" -> StringValue("A")))
      val second     = ObjectValue(List("id" -> StringValue("1")))
      assertTrue(
        projection(ObjectValue(List("_first" -> first, "_second" -> second))) ==
          ObjectValue(List("item" -> ObjectValue(List("name" -> StringValue("A"), "id" -> StringValue("1"))))),
        projection(ObjectValue(List("_first" -> NullValue, "_second" -> second))) == ObjectValue(List("item" -> second))
      )
    },
    test("combines fragment selections for values but uses the first fragment for error paths") {
      val client     = List(
        projectionField("node", "node", List(projectionField("a", "a"))),
        projectionField("node", "node", List(projectionField("b", "b")))
      )
      val executable =
        List(
          projectionField("node", "_node", List(projectionField("a", "_a"))),
          projectionField("node", "_node", List(projectionField("b", "_b")))
        )
      val projection = ResponseProjection.compile(client, executable, Nil, Map.empty)
      assertTrue(
        projection(
          ObjectValue(List("_node" -> ObjectValue(List("_a" -> StringValue("A"), "_b" -> StringValue("B")))))
        ) ==
          ObjectValue(List("node" -> ObjectValue(List("a" -> StringValue("A"), "b" -> StringValue("B"))))),
        projection
          .path(List(PathValue.Key("_node"), PathValue.Key("_a"))) == List(PathValue.Key("node"), PathValue.Key("a")),
        projection.path(List(PathValue.Key("_node"), PathValue.Key("_b"))) == List(
          PathValue.Key("node"),
          PathValue.Key("_b")
        )
      )
    },
    test("translates repeated typename selections once and only transforms selected typenames") {
      val typename   = projectionField("__typename", "kind")
      val fields     = List(typename, typename, projectionField("scalar", "scalar"))
      val payload    = ObjectValue(List("__typename" -> StringValue("A")))
      val projection = ResponseProjection.compile(fields, fields, Nil, Map("A" -> "B", "B" -> "C"))
      val result     =
        projection(ObjectValue(List("kind" -> StringValue("A"), "scalar" -> payload))).asInstanceOf[ObjectValue]
      assertTrue(result.getOrNull("kind") == StringValue("B"), result.getOrNull("scalar") eq payload)
    },
    test("returns untransformed values by reference") {
      val fields               = List(projectionField("name", "name"))
      val value: ResponseValue = ObjectValue(List("name" -> StringValue("A")))
      val projection           = ResponseProjection.compile(fields, fields, Nil, Map("Unused" -> "Renamed"))
      assertTrue(projection(value) eq value)
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
    test("object completion keeps duplicate-field lookup first-wins across the wide threshold") {
      def data(size: Int): ObjectValue =
        ObjectValue(
          ("name"   -> StringValue("first")) ::
            List.tabulate(size - 2)(index => s"field$index" -> StringValue(index.toString)) :::
            ("name" -> StringValue("last")) :: Nil
        )

      assertTrue(
        completion.complete(List(name), data(15), Nil).toResponseValue ==
          ObjectValue(List("name" -> StringValue("first"))),
        completion.complete(List(name), data(16), Nil).toResponseValue ==
          ObjectValue(List("name" -> StringValue("first")))
      )
    },
    test("patches the last duplicate on both sides of the wide threshold") {
      def value(size: Int): ObjectValue =
        ObjectValue(
          ("duplicate"   -> StringValue("first")) ::
            List.tabulate(size - 2)(index => s"field$index" -> StringValue(index.toString)) :::
            ("duplicate" -> StringValue("last")) :: Nil
        )

      val patch  = ObjectValue(("duplicate" -> StringValue("merged")) :: Nil)
      val narrow = ResponseMerge.mergeObject(value(15), patch)
      val wide   = ResponseMerge.mergeObject(value(16), patch)

      def duplicateValues(value: ResponseValue): List[StringValue] =
        value match {
          case ObjectValue(fields) => fields.collect { case ("duplicate", nested: StringValue) => nested }
          case _                   => Nil
        }

      assertTrue(
        duplicateValues(narrow) == List(StringValue("first"), StringValue("merged")),
        duplicateValues(wide) == List(StringValue("first"), StringValue("merged"))
      )
    },
    test("keeps correlation aliases distinct from disambiguated abstract entity fields") {
      val schema =
        """
          |type Query { node: Node }
          |interface Node { label: String }
          |type User implements Node { label: String! }
          |type Admin implements Node { label: String }
          |""".stripMargin

      for {
        graph <- composeSingle("details", schema)
        node   = graph.rootType.types("Node")
        fields = graph.prepareEntityFields(
                   "details",
                   "Node",
                   List(
                     Field(
                       "label",
                       Types.string.nonNull,
                       Some(node),
                       alias = Some("entity_key"),
                       targets = Some(Set("User"))
                     ),
                     Field(
                       "label",
                       Types.string,
                       Some(node),
                       alias = Some("entity_key"),
                       targets = Some(Set("Admin"))
                     )
                   )
                 )
        names  = responseNames(fields)
      } yield assertTrue(
        fields.map(_.aliasedName) == List("_caliban_gateway_entity_key", "_caliban_gateway_entity_key_2"),
        new PrivateAliases(names).next("_caliban_gateway_entity_key") == "_caliban_gateway_entity_key_3"
      )
    },
    test("execution artifacts are reused but variable binding gets an independent cache") {
      val field        = Field("product", objectType, Some(queryType), arguments = Map("id" -> InputValue.VariableValue("id")))
      val fetch        = RootFetch(FetchId(0), "products", List(field), List(field), Nil)
      val plan         = OperationPlan(OperationType.Query, "Query", List(field), Nil, List(fetch), Nil, Nil, None)
      val cache        = plan.executionCache
      val completion   = plan.completion
      val bound        = plan.bind(Map("id" -> StringValue("p1")))
      val projection   = ResponseProjection.compile(Nil, Nil, Nil, Map.empty)
      val originalRoot = PlanExecutor.PreparedRoot("original", projection)
      val boundRoot    = PlanExecutor.PreparedRoot("bound", projection)
      val cachedRoot   = cache.root(fetch.id)(originalRoot)
      assertTrue(
        cache eq plan.executionCache,
        completion eq plan.completion,
        plan.executionCache ne bound.executionCache,
        plan.completion ne bound.completion,
        bound.bind(Map("id" -> StringValue("p2"))) eq bound,
        cachedRoot eq originalRoot,
        cache.root(fetch.id)(boundRoot) eq originalRoot,
        bound.executionCache.root(fetch.id)(boundRoot) eq boundRoot,
        bound.roots.head.downstream.head.arguments == Map("id" -> StringValue("p1")),
        plan.roots.head.downstream.head.arguments == Map("id" -> InputValue.VariableValue("id"))
      )
    }
  )
}
