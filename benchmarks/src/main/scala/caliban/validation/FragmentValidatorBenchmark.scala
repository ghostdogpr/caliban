package caliban.validation

import caliban._
import caliban.parsing.Parser
import caliban.schema.RootType
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FragmentValidatorBenchmark {
  import FragmentValidatorBenchmark._

  @Benchmark
  def fragmentConflicts(): Any =
    run(Validator.validateAll(parsedQuery, rootType))

  @Benchmark
  def eightyFragments(): Any =
    run(Validator.validateAll(parsedEightyFragmentsQuery, rootType))
}

object FragmentValidatorBenchmark {
  import caliban.schema.Schema.auto._
  import caliban.schema.ArgBuilder.auto._

  def run[A](either: Either[Throwable, A]): A = either.fold(throw _, identity)

  case class Owner(name: String, age: Int, address: String)

  sealed trait Pet

  sealed trait DogCommand
  case object DOG_SIT  extends DogCommand
  case object DOG_DOWN extends DogCommand
  case object DOG_HEEL extends DogCommand

  sealed trait CatCommand
  case object CAT_JUMP extends CatCommand
  case object CAT_PURR extends CatCommand

  case class Dog(
    name: String,
    nickname: Option[String],
    barkVolume: Int,
    doesKnowCommand: DogCommand => Boolean,
    owner: Option[Owner]
  ) extends Pet

  case class Cat(
    name: String,
    nickname: Option[String],
    meowVolume: Int,
    doesKnowCommand: CatCommand => Boolean,
    owner: Option[Owner]
  ) extends Pet

  case class Bird(
    name: String,
    nickname: Option[String],
    wingspan: Int,
    owner: Option[Owner]
  ) extends Pet

  case class Fish(
    name: String,
    nickname: Option[String],
    waterType: String,
    owner: Option[Owner]
  ) extends Pet

  case class Query(pet: Pet, pets: List[Pet])

  val api: GraphQL[Any] =
    graphQL(RootResolver(Query(Dog("Rex", Some("Rexie"), 5, _ => true, None), Nil)))

  val rootType: RootType =
    run(api.validateRootSchema.map { schema =>
      RootType(
        schema.query.opType,
        schema.mutation.map(_.opType),
        schema.subscription.map(_.opType)
      )
    })

  // Query designed to stress the shape/parents/groups caches inside
  // FragmentValidator.findConflictsWithinSelectionSet:
  //  - Many inline fragments on a union type (Pet) drive groupByCommonParents
  //    and produce repeated `Set[SelectedField]` shapes (groupsCache hits).
  //  - The same named fragments are spread at multiple nesting levels, so the
  //    same selection-set hashes recur (shapeCache + parentsCache hits).
  //  - Several spreads of the same fragment in a single selection multiply the
  //    number of recursive calls without introducing new selection shapes.
  val query: String = {
    val petSpreads = ("...PetDetails\n" + "...PetOwner\n") * 4
    s"""
       |fragment OwnerDetails on Owner {
       |  name
       |  age
       |  address
       |}
       |
       |fragment PetDetails on Pet {
       |  ... on Dog {
       |    name
       |    nickname
       |    barkVolume
       |    doesKnowCommand(value: DOG_SIT)
       |    owner { ...OwnerDetails }
       |  }
       |  ... on Cat {
       |    name
       |    nickname
       |    meowVolume
       |    doesKnowCommand(value: CAT_JUMP)
       |    owner { ...OwnerDetails }
       |  }
       |  ... on Bird {
       |    name
       |    nickname
       |    wingspan
       |    owner { ...OwnerDetails }
       |  }
       |  ... on Fish {
       |    name
       |    nickname
       |    waterType
       |    owner { ...OwnerDetails }
       |  }
       |}
       |
       |fragment PetOwner on Pet {
       |  ... on Dog {
       |    name
       |    owner { ...OwnerDetails }
       |  }
       |  ... on Cat {
       |    name
       |    owner { ...OwnerDetails }
       |  }
       |  ... on Bird {
       |    name
       |    owner { ...OwnerDetails }
       |  }
       |  ... on Fish {
       |    name
       |    owner { ...OwnerDetails }
       |  }
       |}
       |
       |query {
       |  pet {
       |    $petSpreads
       |  }
       |  pets {
       |    $petSpreads
       |  }
       |}
       |""".stripMargin
  }

  val parsedQuery = run(Parser.parseQuery(query))

  // A 7.5 KB fragment DAG whose operation spreads all 80 fragments. Nested
  // comparisons revisit the same fragment pairs unless they are memoized by name.
  val eightyFragmentsQuery: String = {
    val fragmentCount = 80
    val fragments = (0 until fragmentCount).map { index =>
      val name       = f"F$index%02d"
      val nextSpread =
        if (index + 1 < fragmentCount) f"...F${index + 1}%02d"
        else ""
      s"""fragment $name on Pet {
         |  $nextSpread
         |  ... on Dog { name nickname }
         |  ... on Cat { name }
         |}
         |""".stripMargin
    }.mkString("\n")
    val spreads = (0 until fragmentCount).map(index => f"...F$index%02d").mkString("\n")
    s"""$fragments
       |query {
       |  pet {
       |    $spreads
       |  }
       |}
       |""".stripMargin
  }

  val parsedEightyFragmentsQuery = run(Parser.parseQuery(eightyFragmentsQuery))
}
