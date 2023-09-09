package caliban.wrappers

import caliban.Value.IntValue
import caliban.{ graphQL, ResponseValue, RootResolver, Value }
import caliban.schema.Schema
import caliban.wrappers.Caching.{ CacheHint, CacheScope, GQLCacheControl }
import zio.{ durationInt, UIO, ZIO }
import zio.test.{ assertTrue, ZIOSpecDefault }

object CachingSpec extends ZIOSpecDefault {
  import Fixture._

  val spec = suite("CachingWrapperSpec")(
    test("publicly cached type") {
      val query = """query { publicCachedType { field } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(60),
                  "scope"  -> Value.StringValue("PUBLIC")
                )
              )
            )
          )
        )
      }
    },
    test("privately cached type") {
      val query = """query { privateCachedType { field } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(60),
                  "scope"  -> Value.StringValue("PRIVATE")
                )
              )
            )
          )
        )
      }
    },
    test("publicly cached type with field override") {
      val query = """query { publicCachedOverrideField { field } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(30),
                  "scope"  -> Value.StringValue("PUBLIC")
                )
              )
            )
          )
        )
      }
    },
    test("privately cached type with field override") {
      val query = """query { privateCachedOverrideField { field } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(30),
                  "scope"  -> Value.StringValue("PRIVATE")
                )
              )
            )
          )
        )
      }
    },
    test("most restrictive field wins") {
      val query = """query { mostRestrictiveField { field field2 } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(30),
                  "scope"  -> Value.StringValue("PRIVATE")
                )
              )
            )
          )
        )
      }
    },
    test("most restrictive nested field wins") {
      val query = """query { mostRestrictiveNestedField { field { field } } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(30),
                  "scope"  -> Value.StringValue("PUBLIC")
                )
              )
            )
          )
        )
      }
    },
    test("override field") {
      val query = """query { overrideField { field } }"""
      for {
        res <- api.interpreter.flatMap(_.execute(query))
      } yield {
        val extensions = res.extensions.flatMap(_.fields.collectFirst { case ("cacheControl", v) => v })
        assertTrue(
          extensions.get == ResponseValue.ObjectValue(
            List(
              "version" -> IntValue(2),
              "policy"  -> ResponseValue.ObjectValue(
                List(
                  "maxAge" -> Value.IntValue(10),
                  "scope"  -> Value.StringValue("PRIVATE")
                )
              )
            )
          )
        )
      }
    }
  )

  object Fixture {
    import Schema.auto._
    @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Public))
    case class PublicCachedType(
      field: UIO[String]
    )

    @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Private))
    case class PrivateCachedType(
      field: UIO[String]
    )

    @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Public))
    case class PublicCachedOverrideField(
      @GQLCacheControl(maxAge = Some(30.seconds)) field: UIO[String]
    )

    @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Private))
    case class PrivateCachedOverrideField(
      @GQLCacheControl(maxAge = Some(30.seconds), scope = Some(CacheScope.Private)) field: UIO[String]
    )

    case class MostRestrictiveField(
      @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Public)) field: UIO[String],
      @GQLCacheControl(maxAge = Some(30.seconds), scope = Some(CacheScope.Private)) field2: UIO[String]
    )

    case class NestedField(
      @GQLCacheControl(maxAge = Some(30.seconds)) field: UIO[String]
    )

    case class MostRestrictiveNestedField(
      @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Public)) field: UIO[NestedField]
    )

    @GQLCacheControl(maxAge = Some(1.minute), scope = Some(CacheScope.Public))
    case class Query(
      publicCachedType: PublicCachedType,
      privateCachedType: PrivateCachedType,
      publicCachedOverrideField: PublicCachedOverrideField,
      privateCachedOverrideField: PrivateCachedOverrideField,
      mostRestrictiveField: MostRestrictiveField,
      mostRestrictiveNestedField: MostRestrictiveNestedField,
      overrideField: UIO[PublicCachedType]
    )

    implicit val querySchema: Schema[Any, Query] = Schema.auto.genAll[Any, Query]

    val api = graphQL(
      RootResolver(
        Query(
          PublicCachedType(ZIO.succeed("publicCachedType")),
          PrivateCachedType(ZIO.succeed("privateCachedType")),
          PublicCachedOverrideField(ZIO.succeed("publicCachedOverrideField")),
          PrivateCachedOverrideField(ZIO.succeed("privateCachedOverrideField")),
          MostRestrictiveField(ZIO.succeed("mostRestrictiveField"), ZIO.succeed("mostRestrictiveField2")),
          MostRestrictiveNestedField(ZIO.succeed(NestedField(ZIO.succeed("mostRestrictiveNestedField")))),
          Caching.setCacheHint(CacheHint(Some(10.second), scope = Some(CacheScope.Private))) as PublicCachedType(
            ZIO.succeed("overrideField")
          )
        )
      )
    ) @@ Caching.aspect()

  }
}
