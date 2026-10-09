package caliban.hive

import zio.Config.Secret
import zio._
import zio.http._

/**
 * Settings for [[HiveUsage]].
 *
 * @param token a Hive access token allowed to report usage for `target`
 * @param target the target to report to, as its ID or as `organization/project/target` slugs
 * @param endpoint the usage API; the default is Hive Cloud, a self-hosted Hive serves it under `/usage`
 * @param sampleRate the share of operations to report, from 0 to 1
 * @param exclude names of operations that are never reported, such as health checks
 * @param flushInterval how often collected operations are sent
 * @param maxBatchSize the most operations sent in one report, at least 1
 * @param bufferSize the most operations kept while waiting to be sent, at least 1; further ones are dropped rather than
 *                   slowing requests down
 * @param timeout how long one report may take, and how long releasing the layer waits for the last reports
 */
final case class HiveConfig(
  token: Secret,
  target: String,
  endpoint: URL = HiveConfig.cloudEndpoint,
  sampleRate: Double = 1.0,
  exclude: Set[String] = Set.empty,
  flushInterval: Duration = 5.seconds,
  maxBatchSize: Int = 1000,
  bufferSize: Int = 10000,
  timeout: Duration = 10.seconds
)

object HiveConfig {
  val cloudEndpoint: URL = url"https://app.graphql-hive.com/usage"
}
