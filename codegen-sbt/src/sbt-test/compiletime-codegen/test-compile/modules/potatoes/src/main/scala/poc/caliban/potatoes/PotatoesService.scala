package poc.caliban.potatoes

import zio._
import zio.stream.ZStream

final case class Potato(name: Potato.Name, color: Potato.Color)
object Potato {
  final case class Name(value: String)
  sealed trait Color
  case object White  extends Color
  case object Red    extends Color
  case object Purple extends Color
  case object Yellow extends Color
}

sealed trait PotatoesServiceError extends Throwable
object PotatoesServiceError {
  case object AmericanContinentNotYetDiscovered extends PotatoesServiceError
}

trait PotatoesService {
  def findByName(name: Potato.Name): IO[PotatoesServiceError, Option[Potato]]
  def findByColor(color: Potato.Color): IO[PotatoesServiceError, List[Potato]]
  def makeNewSpecies(name: Potato.Name, color: Potato.Color): IO[PotatoesServiceError, Potato]
  def eradicate(name: Potato.Name): IO[PotatoesServiceError, Unit]
  def all: ZStream[Any, PotatoesServiceError, Potato]
}

object PotatoesService {
  def findByName(name: Potato.Name)                          = ZIO.serviceWithZIO[PotatoesService](_.findByName(name))
  def findByColor(color: Potato.Color)                       = ZIO.serviceWithZIO[PotatoesService](_.findByColor(color))
  def makeNewSpecies(name: Potato.Name, color: Potato.Color) =
    ZIO.serviceWithZIO[PotatoesService](_.makeNewSpecies(name, color))
  def eradicate(name: Potato.Name)                           = ZIO.serviceWithZIO[PotatoesService](_.eradicate(name))
  def all                                                    = ZStream.serviceWithStream[PotatoesService](_.all)
}
