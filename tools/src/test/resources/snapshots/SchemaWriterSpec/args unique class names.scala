object Types {
  final case class HeroCallAlliesArgs(number: Int)
  final case class VillainCallAlliesArgs(number: Int, w: String)
  final case class Hero(callAllies: HeroCallAlliesArgs => List[Hero])
  final case class Villain(callAllies: VillainCallAlliesArgs => List[Villain])

}
