package caliban.execution

import caliban.InputValue
import caliban.InputValue.ObjectValue
import caliban.Value.IntValue
import caliban.schema.{ ArgBuilder, FieldAttributes, GenericSchema, Schema }
import zio.query.ZQuery

object NestedZQueryBenchmarkSchema {

  import Schema._

  type Query[A] = ZQuery[Any, Throwable, A]

  implicit val simpleEntitySchema: Schema[Any, SimpleEntity]       = Schema.gen
  implicit val multipleEntitySchema: Schema[Any, MultifieldEntity] = Schema.gen
  lazy implicit val deepEntitySchema: Schema[Any, DeepEntity]      = obj[Any, DeepEntity]("DeepEntity") { implicit fa =>
    List(
      field[DeepEntity]("next")(_.next),
      field[DeepEntity]("nested")(_.nested)
    )
  }

  implicit val deepArgsBuilder: ArgBuilder[DeepArgs] = ArgBuilder.derived

  lazy implicit val deepArgsSchema: Schema[Any, DeepArgs] =
    obj[Any, DeepArgs]("DeepArgs") { implicit fa =>
      List(
        field[DeepArgs]("i")(_.i),
        field[DeepArgs]("next")(_.next)
      )
    }

  lazy implicit val deepEntityWithArgsSchema: Schema[Any, DeepWithArgsEntity] =
    obj[Any, DeepWithArgsEntity]("DeepWithArgsEntity") { implicit fa =>
      List(
        field[DeepWithArgsEntity]("next")(_.next),
        fieldWithArgs[DeepWithArgsEntity, DeepArgs]("nested")(d => i => d.nested(i))
      )
    }

  implicit val simpleRootSchema: Schema[Any, SimpleRoot]               = Schema.gen
  implicit val multipleRootSchema: Schema[Any, MultifieldRoot]         = Schema.gen
  implicit val nestedRootSchema: Schema[Any, DeepRoot]                 = Schema.gen
  implicit val nestedWithArgsRootSchema: Schema[Any, DeepWithArgsRoot] = Schema.gen

  case class SimpleRoot(entities: Query[List[SimpleEntity]])
  case class SimpleEntity(id: Int, nested: Query[Int])
  case class MultifieldRoot(entities: Query[List[MultifieldEntity]])
  case class MultifieldEntity(
    id: Int,
    nested0: Query[Int],
    nested1: Query[Int],
    nested2: Query[Int],
    nested3: Query[Int],
    nested4: Query[Int]
  )
  case class DeepRoot(entities: Query[List[DeepEntity]])
  case class DeepEntity(next: Query[Option[DeepEntity]], nested: Query[Int])
  case class DeepWithArgsRoot(entities: Query[List[DeepWithArgsEntity]])
  case class DeepArgs(i: Int, next: Option[DeepArgs])
  case class DeepWithArgsEntity(next: Query[Option[DeepWithArgsEntity]], nested: DeepArgs => Query[Int])

  val simple100Elements: SimpleRoot   = generateSimple(100)
  val simple1000Elements: SimpleRoot  = generateSimple(1000)
  val simple10000Elements: SimpleRoot = generateSimple(10000)

  val simpleQuery: String = """{
    entities {
      id
      nested
    }
  }""".stripMargin

  val multifield100Elements: MultifieldRoot   = generateMulti(100)
  val multifield1000Elements: MultifieldRoot  = generateMulti(1000)
  val multifield10000Elements: MultifieldRoot = generateMulti(10000)

  val multifieldQuery: String = """{
    entities {
      id
      nested0
      nested1
      nested2
      nested3
      nested4
    }
  }""".stripMargin

  val deep100Elements: DeepRoot   = generateDeep(100)
  val deep1000Elements: DeepRoot  = generateDeep(1000)
  val deep10000Elements: DeepRoot = generateDeep(10000)

  val deepQuery: String = """{
    entities {
      nested
      next {
        nested
        next {
          nested
          next {
            nested
            next {
              nested
            }
          }
        }
      }
    }
  }""".stripMargin

  val deepWithArgs100Elements: DeepWithArgsRoot   = generateDeepWithArgs(100)
  val deepWithArgs1000Elements: DeepWithArgsRoot  = generateDeepWithArgs(1000)

  val deepArgs100Elements: Map[String, InputValue] = generateDeepArgs(100)
  val deepArgs1000Elements:  Map[String, InputValue] = generateDeepArgs(1000)

  val deepWithArgsQuery: String =
    """query IntrospectionQuery($args: DeepArgsInput!) {
    entities {
      nested(i: $args)
      next {
        nested(i: $args)
        next {
          nested(i: $args)
          next {
            nested(i: $args)
            next {
              nested(i: $args)
            }
          }
        }
      }
    }
  }""".stripMargin

  private def generateSimple(n: Int) = {
    val entities = (1 to n).map(i => SimpleEntity(i, ZQuery.succeed(i))).toList
    SimpleRoot(ZQuery.succeed(entities))
  }

  private def generateMulti(n: Int) = {
    val entities = (1 to n)
      .map(i =>
        MultifieldEntity(
          i,
          ZQuery.succeed(i),
          ZQuery.succeed(i),
          ZQuery.succeed(i),
          ZQuery.succeed(i),
          ZQuery.succeed(i)
        )
      )
      .toList
    MultifieldRoot(ZQuery.succeed(entities))
  }

  private def generateDeep(n: Int) = {
    def loop(n: Int): DeepEntity =
      if (n == 0)
        DeepEntity(ZQuery.none, ZQuery.succeed(n))
      else {
        val next = loop(n - 1)
        DeepEntity(ZQuery.some(next), ZQuery.succeed(n))
      }

    val entities = (1 to n).map(_ => loop(5)).toList
    DeepRoot(ZQuery.succeed(entities))
  }

  private def generateDeepWithArgs(n: Int) = {
    def loop(n: Int): DeepWithArgsEntity =
      if (n == 0)
        DeepWithArgsEntity(ZQuery.none, args => ZQuery.succeed(args.i + n))
      else {
        val next = loop(n - 1)
        DeepWithArgsEntity(ZQuery.some(next), args => ZQuery.succeed(args.i + n))
      }

    val entities = (1 to n).map(_ => loop(5)).toList
    DeepWithArgsRoot(ZQuery.succeed(entities))
  }

  private def generateDeepArgs(n: Int) = {
    def loop(n: Int): Map[String, InputValue] =
      if (n == 0)
        Map("i" -> IntValue(n))
      else {
        val next = loop(n - 1)
        Map("i" -> IntValue(n), "next" -> ObjectValue(next))
      }

    Map("args" -> ObjectValue(loop(n)))
  }
}
