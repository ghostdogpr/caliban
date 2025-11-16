import Types._

object Types {
  final case class HeroNameArgs(pad: Int) derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
  final case class QueryHeroArgs(episode: scala.Option[Episode])
      derives caliban.schema.Schema.SemiAuto,
        caliban.schema.ArgBuilder
  final case class Hero(name: HeroNameArgs => String) derives caliban.schema.Schema.SemiAuto
  final case class HeroInput(name: String) derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder

  sealed trait Episode extends scala.Product with scala.Serializable
      derives caliban.schema.Schema.SemiAuto,
        caliban.schema.ArgBuilder

  object Episode {
    case object NEWHOPE extends Episode derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
    case object EMPIRE  extends Episode derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
    case object JEDI    extends Episode derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder
  }

}

object Operations {

  final case class Query(
    hero: QueryHeroArgs => zio.UIO[scala.Option[Hero]]
  ) derives caliban.schema.Schema.SemiAuto

}
