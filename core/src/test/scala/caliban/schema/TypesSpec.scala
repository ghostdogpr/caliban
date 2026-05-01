package caliban.schema

import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import zio.test._

object TypesSpec extends ZIOSpecDefault {

  private val species: __Type = Types.makeInterface(
    name = Some("Species"),
    description = None,
    fields = () => List(__Field("name", None, _ => Nil, () => Types.string.nonNull)),
    subTypes = Nil
  )

  private val mammalia: __Type = __Type(
    kind = __TypeKind.OBJECT,
    name = Some("Mammalia"),
    interfaces = () => Some(List(species)),
    fields = _ => Some(List(__Field("name", None, _ => Nil, () => Types.string.nonNull)))
  )

  private val aves: __Type = __Type(
    kind = __TypeKind.OBJECT,
    name = Some("Aves"),
    interfaces = () => Some(List(species)),
    fields = _ => Some(List(__Field("name", None, _ => Nil, () => Types.string.nonNull)))
  )

  override def spec = suite("TypesSpec")(
    suite("unify")(
      test("returns the type when both arguments are the same") {
        assertTrue(Types.unify(Types.string, Types.string).contains(Types.string))
      },
      test("preserves NON_NULL when both arguments are the same NON_NULL type") {
        val t = Types.string.nonNull
        assertTrue(Types.unify(t, t).map(_.kind).contains(__TypeKind.NON_NULL))
      },
      test("strips NON_NULL when one argument is nullable") {
        assertTrue(Types.unify(Types.string.nonNull, Types.string).contains(Types.string))
      },
      test("falls back to the closest common interface for distinct sub-types") {
        // Per https://spec.graphql.org/October2021/#IsValidImplementationFieldType() interface fields
        // may be covariantly narrowed by their implementations. unify should yield the shared interface.
        assertTrue(Types.unify(mammalia, aves).flatMap(_.name).contains("Species"))
      },
      test("preserves NON_NULL when both sub-types share an interface") {
        val unified = Types.unify(mammalia.nonNull, aves.nonNull)
        assertTrue(unified.map(_.kind).contains(__TypeKind.NON_NULL)) &&
        assertTrue(unified.flatMap(_.ofType).flatMap(_.name).contains("Species"))
      },
      test("returns None when no common interface exists") {
        val unrelated = __Type(
          kind = __TypeKind.OBJECT,
          name = Some("Unrelated"),
          interfaces = () => Some(Nil)
        )
        assertTrue(Types.unify(mammalia, unrelated).isEmpty)
      }
    )
  )
}
