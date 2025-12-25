package caliban.fastargbuilder

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(
  iterations = 2,
  time = 5,
  batchSize = 10
)
@Measurement(
  iterations = 5,
  time = 5,
  batchSize = 10
)
@Fork(value = 1)
class FastArgBuilderBenchmark {

  private val InputsCount = 10_000

  private var testInputs: Array[caliban.InputValue] = _
  private var currentIndex: Int                     = 0

  @Setup(Level.Trial)
  def setup(): Unit =
    testInputs = (1 to InputsCount).map(_ => HugeCaseClass.random()).toArray

  @Setup(Level.Invocation)
  def setupInvocation(): Unit =
    currentIndex = (currentIndex + 1) % testInputs.length

  @Benchmark
  def benchmarkArgBuilder(blackhole: Blackhole): Unit = {
    val result = HugeCaseClass.argBuilder.build(testInputs(currentIndex))
    blackhole.consume(result)
  }

  @Benchmark
  def benchmarkFastArgBuilder(blackhole: Blackhole): Unit = {
    val result = HugeCaseClass.fastArgBuilder.build(testInputs(currentIndex))
    blackhole.consume(result)
  }
}
