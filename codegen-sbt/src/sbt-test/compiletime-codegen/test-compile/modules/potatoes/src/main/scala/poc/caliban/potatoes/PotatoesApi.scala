package poc.caliban.potatoes

import caliban._
import caliban.schema.GenericSchema
import caliban.wrappers.Wrappers._
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
      byName = name => PotatoesService.findByName(name),
      byColor = color => PotatoesService.findByColor(color)
    )

  private val mutations =
    Mutation(
      makeNewSpecies = args => PotatoesService.makeNewSpecies(args.name, args.color),
      eradicate = name => PotatoesService.eradicate(name)
    )

  private val subscriptions =
    Subscription(
      allPotatoes = PotatoesService.all
    )

  val resolver: RootResolver[Query, Mutation, Subscription] = RootResolver(queries, mutations, subscriptions)
}

object PotatoesApi extends GenericSchema[PotatoesService] {
  import auto._
  import caliban.schema.ArgBuilder.auto._

  val api: GraphQL[PotatoesService] =
    graphQL[PotatoesService, Operations.Query, Operations.Mutation, Operations.Subscription](
      Resolvers.resolver
    ) @@
      maxFields(200) @@
      maxDepth(30) @@
      timeout(5.seconds) @@
      printSlowQueries(500.millis) @@
      printErrors

}
