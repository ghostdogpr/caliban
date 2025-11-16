object Types {

  sealed trait Origin extends scala.Product with scala.Serializable

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
  }

}
