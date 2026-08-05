package caliban.gateway

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.gateway.subgraphs.{ CalibanSubGraph, FederatedSubGraph, GraphQLSubGraph }
import caliban.introspection.adt.{ __Schema, TypeVisitor }
import caliban.parsing.adt.OperationType
import caliban.{ GraphQL, ResponseValue }
import sttp.client4.Backend
import zio.{ Chunk, RIO, Task, ZIO }

trait SubGraph[-R] {
  val name: String
  def build: RIO[R, SubGraphExecutor[R]]
}

object SubGraph {
  trait SubGraphExecutor[-R] {
    val name: String
    val exposeAtRoot: Boolean
    val schema: __Schema
    val visitors: Chunk[TypeVisitor] = Chunk.empty
    def run(field: Field, operationType: OperationType): ZIO[R, ExecutionError, ResponseValue]
  }

  def graphQL(
    name: String,
    url: String,
    headers: Map[String, String] = Map.empty,
    exposeAtRoot: Boolean = true
  ): SubGraph[Backend[Task]] =
    GraphQLSubGraph(name, url, headers, exposeAtRoot)

  def federated(
    name: String,
    url: String,
    headers: Map[String, String] = Map.empty,
    exposeAtRoot: Boolean = true
  ): SubGraph[Backend[Task]] =
    FederatedSubGraph(name, url, headers, exposeAtRoot)

  def caliban[R](name: String, api: GraphQL[R], exposeAtRoot: Boolean = true): SubGraph[R] =
    CalibanSubGraph(name, api, exposeAtRoot)
}
