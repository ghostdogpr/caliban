package caliban.execution

import caliban.CalibanError.ValidationError
import caliban.Macros.gqldoc
import caliban.ResponseValue.StreamValue
import caliban.TestUtils.{ characters, Character }
import caliban.{ CalibanError, GraphQLResponse, HttpUtils, ResponseValue, Value }
import zio.test.Assertion.hasSameElements
import zio.test._
import zio._
import zio.stream.ZStream

trait CharacterService {
  def characterBy(pred: Character => Boolean): UIO[List[Character]]
}

object CharacterService {
  val test: ULayer[CharacterService] = ZLayer.succeed(new CharacterService {
    override def characterBy(pred: Character => Boolean): UIO[List[Character]] =
      ZIO.succeed(characters.filter(pred))
  })
}

object IncrementalDeliverySpec extends ZIOSpecDefault {

  import TestDeferredSchema._

  override def spec = suite("Incremental Delivery")(
    suite("@defer")(
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
        } yield assertTrue(data.size == 1, data.head.toString == """{"data":{"character":{"name":"Roberta Draper"}}}""")
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
          rest.head.toString == """{"data":{"character":{"name":"Roberta Draper"}},"hasNext":true}""",
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
          head  = rest.head.toString
          tail  = rest.tail.map(_.toString)
        } yield assertTrue(
          head == """{"data":{"character":{"name":"Roberta Draper"}},"hasNext":true}""",
          tail == Chunk(
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
          head  = rest.head.toString
          tail  = rest.tail.map(_.toString)
        } yield assertTrue(
          head == """{"data":{"character":{"nicknames":["Bobbie","Gunny"]}}}""",
          tail.isEmpty
        )
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
          head  = rest.head.toString
          tail  = rest.tail.map(_.toString)
        } yield assertTrue(
          head == """{"data":{"character":{}},"hasNext":true}"""
        ) && assert(tail)(
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
          values = rest.map(_.toString)
        } yield assertTrue(
          values == Chunk(
            """{"data":{"character":{"name":"Roberta Draper"}},"hasNext":true}""",
            """{"incremental":[{"data":{"connections":[{"name":"Alex Kamal"}]},"path":["character"],"label":"outer"}],"hasNext":true}""",
            """{"incremental":[{"data":{"nicknames":[]},"path":["character","connections",0],"label":"inner"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      },
      test("deferred fields backed by a datasource") {
        import TestDatasourceDeferredSchema._
        val query = gqldoc("""
        {
          foo {
            bar { ... @defer { value } }
          }
        }""")

        for {
          resp <- interpreter.flatMap(_.execute(query))
          rest <- runIncrementalResponses(resp)
          head  = rest.head.toString
          mid   = rest.tail.init.toList.map(_.toString)
          last  = rest.last.toString
        } yield assertTrue(
          head == """{"data":{"foo":{"bar":[{},{},{}]}},"hasNext":true}""",
          mid.sorted == List(
            """{"incremental":[{"data":{"value":"value"},"path":["foo","bar",0]}],"hasNext":true}""",
            """{"incremental":[{"data":{"value":"value"},"path":["foo","bar",1]}],"hasNext":true}""",
            """{"incremental":[{"data":{"value":"value"},"path":["foo","bar",2]}],"hasNext":true}"""
          ),
          last == """{"hasNext":false}"""
        )
      } @@ TestAspect.nonFlaky(50)
    ),
    suite("@stream")(
      test("don't stream pure values") {
        val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             name
             nicknames @stream(label: "nicknames", initialCount: 2)
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
      test("respect the initial count") {
        val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             name
             quotes @stream(label: "quotes", initialCount: 1) {
               line
             }
           }
        }
          """)

        for {
          response <- interpreter.flatMap(_.execute(query))
          data     <- runIncrementalResponses(response)
          head      = data.map(_.toString)
        } yield assertTrue(
          data.size == 3,
          head == Chunk(
            """{"data":{"character":{"name":"Roberta Draper","quotes":[{"line":"You just have to believe that what you’re doing really matters, and then the fear can’t control you."}]}},"hasNext":true}""",
            """{"incremental":[{"items":[{"line":"I'm Roberta, you can call me Bobby."}],"path":["character","quotes",1],"label":"quotes"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      },
      test("validate step occurs on on lists only") {
        val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             name
             quotes @stream(label: "quotes", initialCount: 1) {
               line
             }
           }
        }
          """)

        for {
          response <- interpreter.flatMap(_.execute(query))
          data     <- runIncrementalResponses(response)
          head      = data.map(_.toString)
        } yield assertTrue(
          data.size == 3,
          head == Chunk(
            """{"data":{"character":{"name":"Roberta Draper","quotes":[{"line":"You just have to believe that what you’re doing really matters, and then the fear can’t control you."}]}},"hasNext":true}""",
            """{"incremental":[{"items":[{"line":"I'm Roberta, you can call me Bobby."}],"path":["character","quotes",1],"label":"quotes"}],"hasNext":true}""",
            """{"hasNext":false}"""
          )
        )
      },
      test("labels must be unique") {
        val query = gqldoc("""
        {
           character(name: "Roberta Draper") {
             name
             quotes @stream(label: "quotes") {
               line
             }
             other: quotes @stream(label: "quotes") {
               line
             }
           }
        }
          """)

        for {
          response <- interpreter.flatMap(_.execute(query))
          errors    = response.errors
        } yield assertTrue(
          errors.size == 1,
          errors.head.is(_.subtype[ValidationError]).msg == "Stream and defer directive labels must be unique"
        )
      }
    )
  ).provide(CharacterService.test, QuoteService.test)

  def runIncrementalResponses(response: GraphQLResponse[CalibanError]) =
    (response match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        stream.via(HttpUtils.DeferMultipart.createPipeline(resp))
      case resp                                                 =>
        ZStream.succeed(resp.toResponseValue)
    }).runCollect
}
