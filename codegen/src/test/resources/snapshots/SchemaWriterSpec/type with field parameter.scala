object Types {
  final case class HeroNameArgs(pad: Int)
  final case class Hero(name: HeroNameArgs => String, nick: String, bday: scala.Option[Int])

}
