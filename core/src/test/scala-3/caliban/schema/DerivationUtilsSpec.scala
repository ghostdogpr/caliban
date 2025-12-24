package caliban.schema

import zio.test.*

object DerivationUtilsSpec extends ZIOSpecDefault {
  override def spec = suite("DerivationUtilsSpec")(
    test("hasSingleField") {
      val n0 = DerivationUtils.hasSingleField[EmptyTuple]
      val n1 = DerivationUtils.hasSingleField[Tuple1[String]]
      val n2 = DerivationUtils.hasSingleField[(String, Int)]

      assertTrue(!n0, n1, !n2)
    }
  )
}
