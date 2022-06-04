package caliban.execution

import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.ResponseValue.StreamValue
import caliban.{ CalibanError, GraphQLResponse, RootResolver }
import caliban.TestUtils.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.TestUtils.{ characters, CaptainShipName, Character, CharacterArgs, Origin, Role }
import caliban.schema.Annotations.GQLName
import caliban.schema.{ GenericSchema, Schema }
import caliban.wrappers.DeferSupport
import zio.test.Assertion.hasSameElements
import zio.test.{ assert, assertTrue, TestAspect, ZIOSpecDefault }
import zio.{ Chunk, UIO, URIO, ZIO, ZLayer }

trait CharacterService {
  def characterBy(pred: Character => Boolean): UIO[List[Character]]
}

object CharacterService {
  val test = ZLayer.succeed(new CharacterService {
    override def characterBy(pred: Character => Boolean): UIO[List[Character]] =
      ZIO.succeed(characters.filter(pred))
  })
}

object DeferredExecutionSpec extends ZIOSpecDefault {

  import TestDeferredSchema._

  override def spec = suite("Defer Execution")(
    test("don't defer pure fields") {

      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer {
               name
             }
           }
        }
          """)

      for {
        response <- interpreter.flatMap(_.execute(query))
        data     <- runIncrementalResponses(response)
      } yield assertTrue(data.size == 1) && assertTrue(
        data.head.toString == """{"character":{"name":"Roberta Draper"}}"""
      )
    },
    test("inline fragments") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer {
               name
               nicknames
             }
           }
        }
          """)

      for {
        resp <- interpreter.flatMap(_.execute(query))
        rest <- runIncrementalResponses(resp)
      } yield assertTrue(
        rest.head.toString == """{"character":{"name":"Roberta Draper"}}"""
      ) && assertTrue(
        rest.tail.toList.map(_.toString) == List(
          """{"incremental":[{"data":{"nicknames":["Bobbie","Gunny"]},"path":["character"]}],"hasNext":true}""",
          """{"hasNext":false}"""
        )
      )
    },
    test("named fragments") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ...Fragment @defer
           }
        }
        
        fragment Fragment on Character {
          name
          nicknames
        }
          """)

      for {
        resp <- interpreter.flatMap(_.execute(query))
        rest <- runIncrementalResponses(resp)
      } yield assertTrue(
        rest.head.toString == """{"character":{"name":"Roberta Draper"}}"""
      ) && assertTrue(
        rest.tail.toList.map(_.toString) == List(
          """{"incremental":[{"data":{"nicknames":["Bobbie","Gunny"]},"path":["character"]}],"hasNext":true}""",
          """{"hasNext":false}"""
        )
      )
    },
    test("disable") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer(if: false, label: "first") { nicknames }
           }
        }
          """)

      for {
        resp <- interpreter.flatMap(_.execute(query))
        rest <- runIncrementalResponses(resp)
      } yield assertTrue(
        rest.head.toString == """{"character":{"nicknames":["Bobbie","Gunny"]}}"""
      ) && assertTrue(rest.tail.isEmpty)
    },
    test("different labels") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             ... @defer(label: "first") { n1: nicknames }
             ... @defer(label: "second") { n2: nicknames }
           }
        }
          """)

      for {
        resp <- interpreter.flatMap(_.execute(query))
        rest <- runIncrementalResponses(resp)
      } yield assertTrue(
        rest.head.toString == """{"character":{}}"""
      ) && assert(rest.tail.toList.map(_.toString))(
        hasSameElements(
          List(
            """{"incremental":[{"data":{"n1":["Bobbie","Gunny"]},"path":["character"],"label":"first"}],"hasNext":true}""",
            """{"incremental":[{"data":{"n2":["Bobbie","Gunny"]},"path":["character"],"label":"second"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      )
    },
    test("nested defers") {
      val query = gqldoc("""
           query test {
             character(name: "Roberta Draper") {
               name
               ... @defer(label: "outer") { 
                  connections(by: Origin) {
                    ...FragmentHuman @defer(label: "inner")
                  }
               }
             }
           }
           
           fragment FragmentHuman on Character {
             name
             nicknames
           }
          """)

      for {
        first <- interpreter.flatMap(_.execute(query))
        rest  <- runIncrementalResponses(first)
      } yield assertTrue(rest.head.toString == """{"character":{"name":"Roberta Draper"}}""") && assertTrue(
        rest.tail.toList.map(_.toString) == List(
          """{"incremental":[{"data":{"connections":[{"name":"Alex Kamal"}]},"path":["character"],"label":"outer"}],"hasNext":true}""",
          """{"incremental":[{"data":{"nicknames":[]},"path":["character","connections",0],"label":"inner"}],"hasNext":true}""",
          """{"hasNext":false}"""
        )
      )
    },
    test("streaming values") {
      val query = gqldoc("""
           query test {
            character(name: "Roberta Draper") {
               name
               nicknames @stream(label: "nicknames", initialCount: 1)
            }
           }
          """)

      for {
        response <- interpreter.flatMap(_.execute(query))
        rest     <- runIncrementalResponses(response)
      } yield assertTrue(
        rest.head.toString == """{"character":{"name":"Roberta Draper","nicknames":["Bobbie","Gunny"]}}"""
      ) && assertTrue(
        rest.tail.toList.map(_.toString) == List(
          """{"incremental":[{"data":"Gunny","path":["character","nicknames",1]}],"label":"nicknames","hasNext":true}""",
          """{"hasNext":false}"""
        )
      )
    } @@ TestAspect.ignore
  ).provide(CharacterService.test)

  def runIncrementalResponses(response: GraphQLResponse[CalibanError]) =
    response.data match {
      case StreamValue(stream) => stream.runCollect
      case value               => ZIO.succeed(Chunk.single(value))
    }
}
