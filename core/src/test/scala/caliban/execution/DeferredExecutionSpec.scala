package caliban.execution

import zio.test.DefaultRunnableSpec
import zio.test.ZSpec
import caliban.Macros.gqldoc
import caliban.GraphQL._
import caliban.wrappers.Wrappers._
import caliban.introspection.adt._
import caliban.introspection._
import caliban.schema._

import caliban.TestUtils.{Role, CharacterArgs}
import zio.test._
import Assertion._
import caliban._
import zio.query.ZQuery
import zio.stream.ZStream
import zio.Chunk
import caliban.execution.deferred._

object DeferredExecutionSpec extends DefaultRunnableSpec {

  case class Actor(
    name: String,
    born: String
  )

  trait Actors {
    def getByName(name: String): UIO[Option[Actor]]
  }

  object Actors {
    def getByName(name: String): URIO[Has[Actors], Option[Actor]] =
      URIO.serviceWith(_.getByName(name))
  }

  case class Character(
    name: String,
    nicknames: List[String],
    origin: Origin,
    role: Option[Role],
    playedBy: UIO[Option[Actor]]
  )

  @GQLDescription("Queries")
  case class Query(
    @GQLDescription("Return all characters from a given origin") character: CharacterArgs => Option[Character]
  )

  object Api extends GenericSchema[Has[Actors]] {

    val characters = List(
      Character(
        "James Holden",
        List("Jim", "Hoss"),
        EARTH,
        Some(Captain(CaptainShipName("Rocinante"))),
        Actors.getByName("Steven Strait")
      ),
      Character(
        "Naomi Nagata",
        Nil,
        BELT,
        Some(Engineer("Rocinante")),
        Actors.getByName("Dominique Tipper")
      )
    )

    implicit val characterSchema: Schema[Has[Actors], Character] = gen[Character]
    
    val resolver = graphQL(RootResolver(
        Query(
          character = args => characters.find(_.name == args.name)
        )
    ))
  }

  import Api._

  override def spec = suite("deferred execution")(
    testM("can execute a deferred query") {
      val query = gqldoc("""
                query test {
                    character(name: "Amos Burton") {
                        ...CharacterFragment @defer(label:"test")
                    }
                }

                fragment CharacterFragment on Character {
                    name
                    nicknames
                }
            """)

      val api = graphQL(resolverIO).withAdditionalDirectives(
        List(
          __Directive(
            "defer",
            None,
            Set(__DirectiveLocation.INLINE_FRAGMENT, __DirectiveLocation.FRAGMENT_SPREAD),
            args = List(
              __InputValue(
                "label",
                None,
                () => Types.makeNonNull(Types.string),
                None
              ),
              __InputValue("skip", None, () => Types.boolean, None)
            )
          )
        )
      ) @@ deferredExecution

      for {
        interpreter   <- api.interpreter
        (first, rest) <- interpreter.execute(query).toDeferred
        values        <- rest.runCollect
      } yield assertTrue(first.data.toString == "{\"character\":{}}") && assertTrue(values == Chunk.empty)
    }
  )

}
