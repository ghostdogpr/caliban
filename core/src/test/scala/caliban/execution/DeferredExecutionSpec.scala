package caliban.execution

import caliban.Macros.gqldoc
import caliban.ResponseValue.StreamValue
import caliban.TestUtils.{ characters, Character }
import caliban.{ CalibanError, GraphQLResponse, ResponseValue, Value }
import zio.test.Assertion.hasSameElements
import zio.test.{ assert, assertTrue, TestAspect, ZIOSpecDefault }
import zio.{ Chunk, UIO, ZIO, ZLayer }

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
      } yield assertTrue(data.size == 1, data.head.toString == """{"character":{"name":"Roberta Draper"}}""")
    },
    test("don't stream if no defer present") {
      val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             name
             nicknames
           }
        }
          """)

      for {
        response <- interpreter.flatMap(_.execute(query))
      } yield assertTrue(
        response.data == ResponseValue.ObjectValue(
          List(
            "character" -> ResponseValue.ObjectValue(
              List(
                "name"      -> Value.StringValue("Roberta Draper"),
                "nicknames" -> ResponseValue.ListValue(
                  List(
                    Value.StringValue("Bobbie"),
                    Value.StringValue("Gunny")
                  )
                )
              )
            )
          )
        )
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
        rest.head.toString == """{"character":{"name":"Roberta Draper"}}""",
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
        rest.head.toString == """{"character":{"name":"Roberta Draper"}}""",
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
      } yield assertTrue(rest.head.toString == """{"character":{"nicknames":["Bobbie","Gunny"]}}""", rest.tail.isEmpty)
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
      } yield assertTrue(
        rest.head.toString == """{"character":{"name":"Roberta Draper"}}""",
        rest.tail.toList.map(_.toString) == List(
          """{"incremental":[{"data":{"connections":[{"name":"Alex Kamal"}]},"path":["character"],"label":"outer"}],"hasNext":true}""",
          """{"incremental":[{"data":{"nicknames":[]},"path":["character","connections",0],"label":"inner"}],"hasNext":true}""",
          """{"hasNext":false}"""
        )
      )
    }
  ).provide(CharacterService.test)

  def runIncrementalResponses(response: GraphQLResponse[CalibanError]) =
    response.data match {
      case StreamValue(stream) => stream.runCollect
      case value               => ZIO.succeed(Chunk.single(value))
    }
}
