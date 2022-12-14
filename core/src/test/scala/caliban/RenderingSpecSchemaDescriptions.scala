package caliban

import caliban.schema.Annotations.GQLDescription

object RenderingSpecSchemaDescriptions {
  case class R1(name: String, @GQLDescription("field. Single line") age: Int)

  case class R2(name: String, @GQLDescription("field.\nMulti line") age: Int)

  case class R3(name: String, @GQLDescription("field. Single line ending in \"quote\"") age: Int)

  case class R4(name: String, @GQLDescription("field.\nMulti line ending in \"quote\"") age: Int)

  case class MyUser1(@GQLDescription("argument single line") id: Int)

  case class MyUser2(@GQLDescription("argument\nMulti line") id: Int)

  case class MyUser3(@GQLDescription("argument single line ending in \"quote\"") id: Int)

  case class MyUser4(@GQLDescription("argument\nMulti line ending in \"quote\"") id: Int)

  case class TheResult(u1: R1, u2: R2, u3: R3, u4: R4)

  case class Query(
    @GQLDescription("query. Single line") getUser1: MyUser1 => TheResult,
    @GQLDescription("query.\nMulti line") getUser2: MyUser2 => TheResult,
    @GQLDescription("query. Single line ending in \"quote\"") getUser3: MyUser3 => TheResult,
    @GQLDescription("query.\nMulti line ending in \"quote\"") getUser4: MyUser4 => TheResult
  )

  def getResult: TheResult = ???

  val resolverForDescriptionTest = RootResolver(
    Query(
      _ => getResult,
      _ => getResult,
      _ => getResult,
      _ => getResult
    )
  )
}
