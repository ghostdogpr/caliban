package poc.caliban.potatoes

import caliban.GraphQL.graphQL
import caliban.schema.GenericSchema
import caliban.wrappers.Wrappers._
import caliban.{GraphQL, RootResolver}
import zio._
import zio.stream.ZStream

import scala.language.higherKinds

object Operations {

  final case class MakeNewSpeciesMutationParams(name: Potato.Name, color: Potato.Color)

  final case class Query(
    byName: Potato.Name => ZIO[PotatoesService, PotatoesServiceError, Option[Potato]],
    byColor: Potato.Color => ZIO[PotatoesService, PotatoesServiceError, List[Potato]]
  )

  final case class Mutation(
    makeNewSpecies: MakeNewSpeciesMutationParams => ZIO[PotatoesService, PotatoesServiceError, Potato],
    eradicate: Potato.Name => ZIO[PotatoesService, PotatoesServiceError, Unit]
  )

  final case class Subscription(
    allPotatoes: ZStream[PotatoesService, PotatoesServiceError, Potato]
  )
}

object Resolvers {
  import Operations._

  private val queries =
    Query(
      byName = name => PotatoesService(_.findByName(name)),
      byColor = color => PotatoesService(_.findByColor(color)),
    )

  private val mutations =
    Mutation(
      makeNewSpecies = args => PotatoesService(_.makeNewSpecies(args.name, args.color)),
      eradicate = name => PotatoesService(_.eradicate(name))
    )

  private val subscriptions =
    Subscription(
      allPotatoes = ZStream.service[PotatoesService].flatMap(_.all)
    )

  val resolver: RootResolver[Query, Mutation, Subscription] = RootResolver(queries, mutations, subscriptions)
}

object Schemas extends GenericSchema[ZEnv with PotatoesService]

object PotatoesApi {
  import Schemas._

  val api: GraphQL[ZEnv with PotatoesService] =
    graphQL(
      Resolvers.resolver
    ) @@
      maxFields(200) @@
      maxDepth(30) @@
      timeout(5.seconds) @@
      printSlowQueries(500.millis) @@
      printErrors

}
