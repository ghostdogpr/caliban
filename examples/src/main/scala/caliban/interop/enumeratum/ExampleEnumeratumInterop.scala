package caliban.interop.enumeratum

import caliban.GraphQL.graphQL
import caliban.RootResolver
import zio.DefaultRuntime

object ExampleEnumeratumInterop {

  import enumeratum._

  //Examples used from Enumeratum README.md
  sealed trait CarType extends EnumEntry

  case object CarType extends Enum[CarType] with CirceEnum[CarType] {

    case object Sedan     extends CarType
    case object SUV       extends CarType
    case object Hatchback extends CarType

    val values = findValues

  }
  import enumeratum.values._

  sealed abstract class EngineType(val value: Int, val name: String) extends IntEnumEntry

  case object EngineType extends IntEnum[EngineType] with IntCirceEnum[EngineType] {

    // A good mix of named, unnamed, named + unordered args
    case object Four   extends EngineType(value = 4, name = "Four Cylinder")
    case object Six    extends EngineType(value = 6, name = "Six Cylinder")
    case object Eight  extends EngineType(8, "Eight Cylinder")
    case object Twelve extends EngineType(12, "Twelve Cylinder")

    val values = findValues

  }

  implicit val runtime = new DefaultRuntime {}

  case class Queries(carTypes: List[CarType], engineTypes: List[EngineType])

  val carTypes    = CarType.values.toList
  val engineTypes = EngineType.values.toList

  val queries     = Queries(carTypes, engineTypes)
  val api         = graphQL(RootResolver(queries))
  val interpreter = api.interpreter

  val query = """
  {
    carTypes
    engineTypes
  }"""

  def run = {
    val runner = for {
      result <- interpreter.execute(query)
      _      <- zio.console.putStrLn(result.data.toString)
    } yield ()
  }
}
