package caliban.parsing

import caliban.parsing.adt.Selection
import zio.test._

object SelectionSpec extends ZIOSpecDefault {

  private val field1: Selection = Selection.Field(
    alias = None,
    name = "id",
    arguments = Map.empty,
    directives = List.empty,
    selectionSet = List.empty,
    index = 0
  )

  private val field2: Selection = Selection.Field(
    alias = None,
    name = "id",
    arguments = Map.empty,
    directives = List.empty,
    selectionSet = List.empty,
    index = 0
  )

  def spec = suite("SelectionSpec")(
    suite("consistent hashCode")(
      test("Field") {
        assertTrue(
          field1 == field2,
          field1.hashCode == field2.hashCode
        )
      },
      test("FragmentSpread") {
        val fragment1 = Selection.FragmentSpread(
          name = "id",
          directives = List.empty
        )

        val fragment2 = Selection.FragmentSpread(
          name = "id",
          directives = List.empty
        )

        assertTrue(
          fragment1 == fragment2,
          fragment1.hashCode == fragment2.hashCode
        )
      },
      test("InlineFragment") {
        val fragment1 = Selection.InlineFragment(
          typeCondition = None,
          dirs = Nil,
          selectionSet = List(field1, field2)
        )

        val fragment2 = Selection.InlineFragment(
          typeCondition = None,
          dirs = Nil,
          selectionSet = List(field1, field2)
        )

        assertTrue(
          fragment1 == fragment2,
          fragment1.hashCode == fragment2.hashCode
        )
      }
    )
  )
}
