package caliban

import caliban.schema.Annotations._

object FragmentSchema {

  @GQLDescription("Queries")
  case class Query(
    bar: Option[Bar],
    foo: List[Foo]
  )

  case class Foo(
    id: String
  )
  case class Bar(
    id: String,
    baz: Option[Baz]
  )

  case class Baz(
    id: String,
    foo: Option[Foo]
  )

  val resolverFooBar: RootResolver[Query, Unit, Unit] = RootResolver(
    Query(
      bar = Some(Bar("1", Some(Baz("2", Some(Foo("3")))))),
      foo = List(Foo("1"), Foo("2"))
    )
  )
}
