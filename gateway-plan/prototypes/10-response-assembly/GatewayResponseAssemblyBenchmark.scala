package caliban.gateway.prototype

import caliban.ResponseValue
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, NullValue }
import com.github.plokhotnyuk.jsoniter_scala.core._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer

/**
 * A throwaway representation spike. The two specialized readers stand in for a
 * compiled source-result program: the operation plan has already assigned the
 * selected response fields and their types.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class GatewayResponseAssemblyBenchmark {
  @Param(Array("join-heavy", "error-heavy"))
  var workload: String = _

  @Param(Array("128"))
  var productCount: Int = _

  private var rootBytes: Array[Byte]   = _
  private var reviewBytes: Array[Byte] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val errorEvery = if (workload == "error-heavy") 3 else 0
    rootBytes = Fixture.root(productCount, errorEvery)
    reviewBytes = Fixture.reviews(productCount)
  }

  @Benchmark
  def responseValue(bh: Blackhole): Unit = bh.consume(responseValueResult())

  @Benchmark
  def indexedMaterialized(bh: Blackhole): Unit = bh.consume(indexedResult())

  @Benchmark
  def rawIndexed(bh: Blackhole): Unit = bh.consume(rawIndexedResult())

  private def responseValueResult(): Array[Byte] = {
    val root        = objectValue(readFromArray[ResponseValue](rootBytes))
    val review      = objectValue(readFromArray[ResponseValue](reviewBytes))
    val products    = listValue(objectValue(root.get("data")).get("products")).values
    val entities    = listValue(objectValue(review.get("data")).get("_entities")).values
    val outputItems = products.zip(entities).map { case (product, entity) =>
      val productObject = objectValue(product)
      if (productObject.get("price") == NullValue) NullValue
      else {
        val merged = objectValue(productObject.deepMerge(entity))
        ObjectValue(
          List("id", "name", "price", "metadata", "reviews").map(key => key -> merged.get(key))
        )
      }
    }
    val data        = ObjectValue(List("products" -> ListValue(outputItems)))
    val errors      = root.get("errors")
    val fields      = if (errors == NullValue) List("data" -> data) else List("data" -> data, "errors" -> errors)
    writeToArray[ResponseValue](ObjectValue(fields))
  }

  private def objectValue(value: ResponseValue): ObjectValue = value match {
    case value: ObjectValue => value
    case _                  => throw new IllegalStateException("Expected an object value in the benchmark fixture.")
  }

  private def listValue(value: ResponseValue): ListValue = value match {
    case value: ListValue => value
    case _                => throw new IllegalStateException("Expected a list value in the benchmark fixture.")
  }

  private def indexedResult(): Array[Byte] = {
    val root    = readFromArray[MaterializedRoot](rootBytes)(MaterializedRoot.codec)
    val reviews = readFromArray[MaterializedReviews](reviewBytes)(MaterializedReviews.codec)
    writeToArray(MaterializedOutput(root, reviews))(MaterializedOutput.codec)
  }

  private def rawIndexedResult(): Array[Byte] = {
    val root    = RawReader.root(rootBytes)
    val reviews = RawReader.reviews(reviewBytes)
    RawOutput.write(RawOutput(root, reviews))
  }
}

private final case class MaterializedRoot(
  ids: Array[String],
  names: Array[String],
  prices: Array[Int],
  metadata: Array[Array[Byte]],
  errors: Array[Byte]
)

private object MaterializedRoot {
  val codec: JsonValueCodec[MaterializedRoot] = new JsonValueCodec[MaterializedRoot] {
    def nullValue: MaterializedRoot                                 = null
    def encodeValue(value: MaterializedRoot, out: JsonWriter): Unit = out.encodeError("decode only")

    def decodeValue(in: JsonReader, default: MaterializedRoot): MaterializedRoot = {
      val ids                 = ArrayBuffer.empty[String]
      val names               = ArrayBuffer.empty[String]
      val prices              = ArrayBuffer.empty[Int]
      val metadata            = ArrayBuffer.empty[Array[Byte]]
      var errors: Array[Byte] = null

      JsonRead.obj(in) { key =>
        if (key == "data")
          JsonRead.obj(in) { dataKey =>
            if (dataKey == "products")
              JsonRead.array(in) { () =>
                var id: String        = null
                var name: String      = null
                var price             = Int.MinValue
                var meta: Array[Byte] = null
                JsonRead.obj(in) { productKey =>
                  productKey match {
                    case "id"       => id = in.readString(null)
                    case "name"     => name = in.readString(null)
                    case "price"    =>
                      val token = in.nextToken()
                      if (token == 'n') in.readNullOrError((), "expected price")
                      else {
                        in.rollbackToken()
                        price = in.readInt()
                      }
                    case "metadata" => meta = in.readRawValAsBytes()
                    case _          => in.skip()
                  }
                }
                ids += id
                names += name
                prices += price
                metadata += meta
              }
            else in.skip()
          }
        else if (key == "errors") errors = in.readRawValAsBytes()
        else in.skip()
      }
      MaterializedRoot(ids.toArray, names.toArray, prices.toArray, metadata.toArray, errors)
    }
  }
}

private final case class MaterializedReviews(values: Array[Array[Byte]])

private object MaterializedReviews {
  val codec: JsonValueCodec[MaterializedReviews] = new JsonValueCodec[MaterializedReviews] {
    def nullValue: MaterializedReviews                                 = null
    def encodeValue(value: MaterializedReviews, out: JsonWriter): Unit = out.encodeError("decode only")

    def decodeValue(in: JsonReader, default: MaterializedReviews): MaterializedReviews = {
      val reviews = ArrayBuffer.empty[Array[Byte]]
      JsonRead.obj(in) { key =>
        if (key == "data")
          JsonRead.obj(in) { dataKey =>
            if (dataKey == "_entities")
              JsonRead.array(in) { () =>
                var value: Array[Byte] = null
                JsonRead.obj(in) { entityKey =>
                  if (entityKey == "reviews") value = in.readRawValAsBytes()
                  else in.skip()
                }
                reviews += value
              }
            else in.skip()
          }
        else in.skip()
      }
      MaterializedReviews(reviews.toArray)
    }
  }
}

private object JsonRead {
  def obj(in: JsonReader)(field: String => Unit): Unit = {
    if (!in.isNextToken('{')) in.decodeError("expected object")
    if (!in.isNextToken('}')) {
      in.rollbackToken()
      var more = true
      while (more) {
        field(in.readKeyAsString())
        more = in.isNextToken(',')
      }
      if (!in.isCurrentToken('}')) in.objectEndOrCommaError()
    }
  }

  def array(in: JsonReader)(item: () => Unit): Unit = {
    if (!in.isNextToken('[')) in.decodeError("expected array")
    if (!in.isNextToken(']')) {
      in.rollbackToken()
      var more = true
      while (more) {
        item()
        more = in.isNextToken(',')
      }
      if (!in.isCurrentToken(']')) in.arrayEndOrCommaError()
    }
  }
}

private final case class MaterializedOutput(root: MaterializedRoot, reviews: MaterializedReviews)

private object MaterializedOutput {
  val codec: JsonValueCodec[MaterializedOutput] = new JsonValueCodec[MaterializedOutput] {
    def nullValue: MaterializedOutput                                                = null
    def decodeValue(in: JsonReader, default: MaterializedOutput): MaterializedOutput =
      in.decodeError("encode only")

    def encodeValue(value: MaterializedOutput, out: JsonWriter): Unit = {
      val root = value.root
      out.writeObjectStart()
      out.writeKey("data")
      out.writeObjectStart()
      out.writeKey("products")
      out.writeArrayStart()
      var i    = 0
      while (i < root.ids.length) {
        if (root.prices(i) == Int.MinValue) out.writeNull()
        else {
          out.writeObjectStart()
          out.writeKey("id")
          out.writeVal(root.ids(i))
          out.writeKey("name")
          out.writeVal(root.names(i))
          out.writeKey("price")
          out.writeVal(root.prices(i))
          out.writeKey("metadata")
          out.writeRawVal(root.metadata(i))
          out.writeKey("reviews")
          out.writeRawVal(value.reviews.values(i))
          out.writeObjectEnd()
        }
        i += 1
      }
      out.writeArrayEnd()
      out.writeObjectEnd()
      if (root.errors ne null) {
        out.writeKey("errors")
        out.writeRawVal(root.errors)
      }
      out.writeObjectEnd()
    }
  }
}

private object Slice {
  val Missing: Long = -1L

  def apply(start: Int, end: Int): Long                = (start.toLong << 32) | (end.toLong & 0xffffffffL)
  def start(value: Long): Int                          = (value >>> 32).toInt
  def end(value: Long): Int                            = value.toInt
  def isNull(bytes: Array[Byte], value: Long): Boolean =
    end(value) - start(value) == 4 && bytes(start(value)) == 'n'
}

private final case class RawRoot(
  bytes: Array[Byte],
  ids: Array[Long],
  names: Array[Long],
  prices: Array[Long],
  metadata: Array[Long],
  errors: Long
)
private final case class RawReviews(bytes: Array[Byte], values: Array[Long])
private final case class RawOutput(root: RawRoot, reviews: RawReviews)

private object RawOutput {
  private val Prefix        = ascii("{\"data\":{\"products\":[")
  private val ProductPrefix = ascii("{\"id\":")
  private val Name          = ascii(",\"name\":")
  private val Price         = ascii(",\"price\":")
  private val Metadata      = ascii(",\"metadata\":")
  private val Reviews       = ascii(",\"reviews\":")
  private val Null          = ascii("null")
  private val DataSuffix    = ascii("]}")
  private val Errors        = ascii(",\"errors\":")

  def write(value: RawOutput): Array[Byte] = {
    val root = value.root
    val out  = new ByteOutput(root.bytes.length + value.reviews.bytes.length)
    out.append(Prefix)
    var i    = 0
    while (i < root.ids.length) {
      if (i > 0) out.append(',')
      if (Slice.isNull(root.bytes, root.prices(i))) out.append(Null)
      else {
        out.append(ProductPrefix)
        out.append(root.bytes, root.ids(i))
        out.append(Name)
        out.append(root.bytes, root.names(i))
        out.append(Price)
        out.append(root.bytes, root.prices(i))
        out.append(Metadata)
        out.append(root.bytes, root.metadata(i))
        out.append(Reviews)
        out.append(value.reviews.bytes, value.reviews.values(i))
        out.append('}')
      }
      i += 1
    }
    out.append(DataSuffix)
    if (root.errors != Slice.Missing) {
      out.append(Errors)
      out.append(root.bytes, root.errors)
    }
    out.append('}')
    out.result()
  }

  private def ascii(value: String): Array[Byte] = value.getBytes(StandardCharsets.US_ASCII)
}

private final class ByteOutput(initialSize: Int) {
  private var bytes = new Array[Byte](initialSize)
  private var index = 0

  def append(value: Array[Byte]): Unit              = append(value, 0, value.length)
  def append(bytes: Array[Byte], slice: Long): Unit =
    append(bytes, Slice.start(slice), Slice.end(slice) - Slice.start(slice))

  def append(value: Char): Unit = {
    ensure(1)
    bytes(index) = value.toByte
    index += 1
  }

  def result(): Array[Byte] = java.util.Arrays.copyOf(bytes, index)

  private def append(value: Array[Byte], offset: Int, length: Int): Unit = {
    ensure(length)
    System.arraycopy(value, offset, bytes, index, length)
    index += length
  }

  private def ensure(additional: Int): Unit =
    if (index + additional > bytes.length) {
      var next = bytes.length << 1
      while (next < index + additional) next = next << 1
      bytes = java.util.Arrays.copyOf(bytes, next)
    }
}

private object RawReader {
  def root(bytes: Array[Byte]): RawRoot = {
    val cursor   = new ByteCursor(bytes)
    val ids      = ArrayBuffer.empty[Long]
    val names    = ArrayBuffer.empty[Long]
    val prices   = ArrayBuffer.empty[Long]
    val metadata = ArrayBuffer.empty[Long]
    var errors   = Slice.Missing
    cursor.obj { key =>
      if (key == "data")
        cursor.obj { dataKey =>
          if (dataKey == "products")
            cursor.array { () =>
              var id    = Slice.Missing
              var name  = Slice.Missing
              var price = Slice.Missing
              var meta  = Slice.Missing
              cursor.obj { productKey =>
                productKey match {
                  case "id"       => id = cursor.capture()
                  case "name"     => name = cursor.capture()
                  case "price"    => price = cursor.capture()
                  case "metadata" => meta = cursor.capture()
                  case _          => cursor.skipValue()
                }
              }
              ids += id
              names += name
              prices += price
              metadata += meta
            }
          else cursor.skipValue()
        }
      else if (key == "errors") errors = cursor.capture()
      else cursor.skipValue()
    }
    RawRoot(bytes, ids.toArray, names.toArray, prices.toArray, metadata.toArray, errors)
  }

  def reviews(bytes: Array[Byte]): RawReviews = {
    val cursor = new ByteCursor(bytes)
    val values = ArrayBuffer.empty[Long]
    cursor.obj { key =>
      if (key == "data")
        cursor.obj { dataKey =>
          if (dataKey == "_entities")
            cursor.array { () =>
              var reviews = Slice.Missing
              cursor.obj { entityKey =>
                if (entityKey == "reviews") reviews = cursor.capture()
                else cursor.skipValue()
              }
              values += reviews
            }
          else cursor.skipValue()
        }
      else cursor.skipValue()
    }
    RawReviews(bytes, values.toArray)
  }
}

/** Minimal JSON cursor used only to test the raw-slice representation. */
private final class ByteCursor(val bytes: Array[Byte]) {
  private var index = 0

  def obj(field: String => Unit): Unit = {
    expect('{')
    whitespace()
    if (peek('}')) index += 1
    else {
      var more = true
      while (more) {
        val key = readString()
        expect(':')
        field(key)
        whitespace()
        if (peek(',')) {
          index += 1
          whitespace()
        } else {
          expect('}')
          more = false
        }
      }
    }
  }

  def array(item: () => Unit): Unit = {
    expect('[')
    whitespace()
    if (peek(']')) index += 1
    else {
      var more = true
      while (more) {
        item()
        whitespace()
        if (peek(',')) {
          index += 1
          whitespace()
        } else {
          expect(']')
          more = false
        }
      }
    }
  }

  def capture(): Long = {
    whitespace()
    val start = index
    skipValue()
    Slice(start, index)
  }

  def skipValue(): Unit = {
    whitespace()
    bytes(index) match {
      case '"' => skipString()
      case '{' =>
        index += 1
        whitespace()
        if (peek('}')) index += 1
        else {
          var done = false
          while (!done) {
            skipString()
            expect(':')
            skipValue()
            whitespace()
            if (peek(',')) index += 1
            else {
              expect('}')
              done = true
            }
          }
        }
      case '[' =>
        index += 1
        whitespace()
        if (peek(']')) index += 1
        else {
          var done = false
          while (!done) {
            skipValue()
            whitespace()
            if (peek(',')) index += 1
            else {
              expect(']')
              done = true
            }
          }
        }
      case _   =>
        while (index < bytes.length && !isDelimiter(bytes(index))) index += 1
    }
  }

  private def readString(): String = {
    whitespace()
    if (!peek('"')) throw new IllegalArgumentException("expected string")
    index += 1
    val start = index
    while (!peek('"')) {
      if (peek('\\')) index += 1
      index += 1
    }
    val value = new String(bytes, start, index - start, StandardCharsets.UTF_8)
    index += 1
    value
  }

  private def skipString(): Unit = {
    whitespace()
    if (!peek('"')) throw new IllegalArgumentException("expected string")
    index += 1
    var done = false
    while (!done)
      if (peek('\\')) index += 2
      else if (peek('"')) {
        index += 1
        done = true
      } else index += 1
  }

  private def whitespace(): Unit =
    while (
      index < bytes.length && (bytes(index) == ' ' || bytes(index) == '\n' || bytes(index) == '\r' || bytes(
        index
      ) == '\t')
    )
      index += 1

  private def expect(char: Char): Unit = {
    whitespace()
    if (!peek(char)) throw new IllegalArgumentException(s"expected ${char.toChar} at $index")
    index += 1
  }

  private def peek(char: Char): Boolean        = index < bytes.length && bytes(index) == char
  private def isDelimiter(char: Byte): Boolean =
    char == ',' || char == '}' || char == ']' || char == ' ' || char == '\n' || char == '\r' || char == '\t'
}

private object Fixture {
  def root(count: Int, errorEvery: Int): Array[Byte] = {
    val out = new StringBuilder(count * 160)
    out.append("{\"data\":{\"products\":[")
    var i   = 0
    while (i < count) {
      if (i > 0) out.append(',')
      out.append("{\"id\":\"p").append(i).append("\",\"name\":\"Product ").append(i).append("\",\"price\":")
      if (errorEvery > 0 && i % errorEvery == 0) out.append("null") else out.append(1000 + i)
      out.append(",\"metadata\":{\"sku\":\"SKU-").append(i).append("\",\"dimensions\":[10,20,30],\"active\":true}}")
      i += 1
    }
    out.append("]}}")
    if (errorEvery > 0) {
      out.setLength(out.length - 1)
      out.append(",\"errors\":[")
      var first = true
      i = 0
      while (i < count) {
        if (i % errorEvery == 0) {
          if (!first) out.append(',')
          first = false
          out.append("{\"message\":\"price unavailable\",\"path\":[\"products\",").append(i).append(",\"price\"]}")
        }
        i += 1
      }
      out.append("]}")
    }
    out.result().getBytes(StandardCharsets.UTF_8)
  }

  def reviews(count: Int): Array[Byte] = {
    val out = new StringBuilder(count * 300)
    out.append("{\"data\":{\"_entities\":[")
    var i   = 0
    while (i < count) {
      if (i > 0) out.append(',')
      out.append("{\"reviews\":[")
      var review = 0
      while (review < 4) {
        if (review > 0) out.append(',')
        out
          .append("{\"body\":\"Review ")
          .append(review)
          .append(" for p")
          .append(i)
          .append("\",\"rating\":")
          .append((review % 5) + 1)
          .append(",\"extra\":{\"verified\":true,\"votes\":[1,2,3]}}")
        review += 1
      }
      out.append("]}")
      i += 1
    }
    out.append("]}}")
    out.result().getBytes(StandardCharsets.UTF_8)
  }
}
