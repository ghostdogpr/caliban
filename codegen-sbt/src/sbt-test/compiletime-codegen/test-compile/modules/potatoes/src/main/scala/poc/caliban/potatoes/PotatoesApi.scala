package poc.caliban.potatoes

import caliban.GraphQL.graphQL
import caliban.schema.GenericSchema
import caliban.wrappers.Wrappers._
import caliban.{GraphQL, RootResolver}
import zio._
import zio.duration.durationInt
import zio.stream.ZStream

import scala.language.higherKinds

object Operations {

  final case class MakeNewSpeciesMutationParams(name: Potato.Name, color: Potato.Color)

  final case class Query(
    byName: Potato.Name => ZIO[Has[PotatoesService], PotatoesServiceError, Option[Potato]],
    byColor: Potato.Color => ZIO[Has[PotatoesService], PotatoesServiceError, List[Potato]]
  )

  final case class Mutation(
    makeNewSpecies: MakeNewSpeciesMutationParams => ZIO[Has[PotatoesService], PotatoesServiceError, Potato],
    eradicate: Potato.Name => ZIO[Has[PotatoesService], PotatoesServiceError, Unit]
  )

  final case class Subscription(
    allPotatoes: ZStream[Has[PotatoesService], PotatoesServiceError, Potato]
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

object Schemas extends GenericSchema[ZEnv with Has[PotatoesService]]

object PotatoesApi {
  import Schemas._

  val api: GraphQL[ZEnv with Has[PotatoesService]] =
    graphQL(
      Resolvers.resolver
    ) @@
      maxFields(200) @@
      maxDepth(30) @@
      timeout(5.seconds) @@
      printSlowQueries(500.millis) @@
      printErrors

}
