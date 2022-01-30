package caliban.reporting

import caliban.GraphQL
import zio.random.Random
import zio.{ random, RIO, Task, ZIO }

import java.security.MessageDigest
import java.util.UUID

/**
 * @param bootId A randomly generated UUID that's unique for each instance of your edge server. Set this value on server startup (a given value should not persist across restarts).
 * @param graphRef Indicates which Apollo Studio graph and variant the server is reporting its schema to (e.g., my-graph-id@my-variant)
 * @param coreSchema The full schema SDL of the graph that is being reported.
 * @param coreSchemaHash The hexadecimal string representation of the normalized schema document's SHA-256 hash
 * @param platform The infrastructure environment that your edge server is running in (localhost, kubernetes/deployment, aws lambda, google cloud run, google cloud function, AWS ECS, etc.)
 * @param serverId An ID that's unique for each instance of your edge server. Unlike bootId, this value should persist across an instance's restarts. In a Kubernetes cluster, this might be the pod name, whereas the container can restart.
 * @param userVersion An arbitrary string you can set to distinguish data sent by different versions of your edge server. For example, this can be the SHA of the Git commit for your deployed server code. We plan to make this value visible in Apollo Studio.
 * @param runtimeVersion The runtime that your edge server is running, such as node 12.03.
 * @param libraryVersion The name and version of the server and/or reporting agent your edge server is using, such as apollo-server-2.8 or graphql-java-3.1.
 */
case class SchemaReportingRef(
  bootId: UUID,
  graphRef: String,
  coreSchema: String,
  coreSchemaHash: String,
  platform: Option[String] = None,
  serverId: Option[String] = None,
  userVersion: Option[String] = None,
  runtimeVersion: Option[String] = None,
  libraryVersion: Option[String] = None
)

object SchemaReportingRef {
  def make(
    schema: GraphQL[Any],
    graphRef: String,
    platform: Option[String] = None,
    serverId: Option[String] = None,
    userVersion: Option[String] = None,
    runtimeVersion: Option[String] = None,
    libraryVersion: Option[String] = None
  ): RIO[Random, SchemaReportingRef] = for {
    bootId         <- random.nextUUID
    rendered        = schema.render
    coreSchemaHash <- hashSchema(rendered)
  } yield SchemaReportingRef(
    bootId,
    coreSchemaHash = coreSchemaHash,
    graphRef = graphRef,
    coreSchema = rendered,
    platform = platform,
    serverId = serverId,
    userVersion = userVersion,
    runtimeVersion = runtimeVersion,
    libraryVersion = libraryVersion
  )

  private def hashSchema(schema: String): Task[String] =
    ZIO.effect(
      MessageDigest
        .getInstance("SHA-256")
        .digest(schema.getBytes("UTF-8"))
        .map("%02x".format(_))
        .mkString
    )

}
