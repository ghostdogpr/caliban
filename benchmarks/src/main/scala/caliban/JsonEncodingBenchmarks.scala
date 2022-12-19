package caliban

import caliban.Value._
import caliban.interop.circe.json.GraphQLResponseCirce.{ graphQLResponseEncoder => circeEncoder }
import caliban.interop.zio.GraphQLResponseZioJson.{ graphQLResponseEncoder => zioEncoder }
import com.github.plokhotnyuk.jsoniter_scala.core._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class JsonEncodingBenchmarks {

  private def mkObject(depth: Int, lengthMultiplier: Int): ResponseValue = {
    val depthM1 = depth - 1
    if (depthM1 <= 0) ResponseValue.ObjectValue(Nil)
    else {
      ResponseValue.ObjectValue(
        List(
          "list"   -> ResponseValue.ListValue(
            List
              .fill(lengthMultiplier)(
                List(
                  StringValue("something"),
                  IntValue(10),
                  mkObject(depthM1, lengthMultiplier),
                  NullValue,
                  BooleanValue(true)
                )
              )
              .flatten
          ),
          "string" -> StringValue("some-string"),
          "int"    -> IntValue(42),
          "bool"   -> BooleanValue(false),
          "object" -> mkObject(depthM1, lengthMultiplier)
        )
      )
    }
  }

  private val testData: GraphQLResponse[Any] = GraphQLResponse(mkObject(5, 20), Nil, None)

  @Benchmark
  def circe(): Unit = circeEncoder(testData).noSpaces

  @Benchmark
  def jsoniter(): Unit = writeToString(testData)

  @Benchmark
  def zio(): Unit = zioEncoder.encodeJson(testData)

}
