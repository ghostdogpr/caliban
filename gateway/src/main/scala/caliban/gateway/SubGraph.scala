package caliban.gateway

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.gateway.subgraphs.{CalibanSubGraph, GraphQLSubGraph}
import caliban.introspection.adt.__Schema
import caliban.tools.SttpClient
import caliban.{GraphQL, ResponseValue}
import zio.{RIO, ZIO}

trait SubGraph[-R] {
  val name: String
  def build: RIO[R, SubGraphExecutor[R]]
}

object SubGraph {
  trait SubGraphExecutor[-R] {
    val name: String
    val exposeAtRoot: Boolean
    val schema: __Schema
    def run(field: Field): ZIO[R, ExecutionError, ResponseValue]
  }

  def graphQL(
    name: String,
    url: String,
    headers: Map[String, String] = Map.empty,
    exposeAtRoot: Boolean = true
  ): SubGraph[SttpClient] =
    GraphQLSubGraph(name, url, headers, exposeAtRoot)

  def caliban[R](name: String, api: GraphQL[R], exposeAtRoot: Boolean = true): SubGraph[R] =
    CalibanSubGraph(name, api, exposeAtRoot)
}
