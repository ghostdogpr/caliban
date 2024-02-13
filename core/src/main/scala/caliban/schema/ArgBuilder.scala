package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value._
import caliban.parsing.Parser
import caliban.uploads.Upload
import zio.Chunk

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.util.UUID
import scala.annotation.implicitNotFound
import scala.collection.immutable.BitSet
import scala.util.Try
import scala.util.control.{ NoStackTrace, NonFatal }

/**
 * Typeclass that defines how to build an argument of type `T` from an input [[caliban.InputValue]].
 * Every type that can be passed as an argument needs an instance of `ArgBuilder`.
 */
@implicitNotFound(
  """Cannot find an ArgBuilder for type ${T}.

Caliban provides instances of ArgBuilder for the most common Scala types, and can derive it for your case classes and sealed traits.
Derivation requires that you have a Schema for any other type nested inside ${T}.
See https://ghostdogpr.github.io/caliban/docs/schema.html for more information.
"""
)
trait ArgBuilder[T] { self =>

  /**
   * Builds a value of type `T` from an input [[caliban.InputValue]].
   * Fails with an [[caliban.CalibanError.ExecutionError]] if it was impossible to build the value.
   */
  def build(input: InputValue): Either[ExecutionError, T]

  /**
   * Builds a value of type `T` from a missing input value.
   * By default, this delegates to [[build]], passing it NullValue.
   * Fails with an [[caliban.CalibanError.ExecutionError]] if it was impossible to build the value.
   */
  def buildMissing(default: Option[String]): Either[ExecutionError, T] =
    default
      .map(
        Parser
          .parseInputValue(_)
          .flatMap(build)
          .left
          .map(e => ExecutionError(e.getMessage(), innerThrowable = Some(InvalidInputArgument)))
      )
      .getOrElse(build(NullValue))

  /**
   * Builds a new `ArgBuilder` of `A` from an existing `ArgBuilder` of `T` and a function from `T` to `A`.
   * @param f a function from `T` to `A`.
   */
  def map[A](f: T => A): ArgBuilder[A] = (input: InputValue) => self.build(input).map(f)

  /**
   * Builds a new `ArgBuilder` of A from an existing `ArgBuilder` of `T` and a function from `T` to `Either[ExecutionError, A]`.
   * @param f a function from `T` to Either[ExecutionError, A]
   */
  def flatMap[A](f: T => Either[ExecutionError, A]): ArgBuilder[A] = (input: InputValue) => self.build(input).flatMap(f)

  /**
   * Builds a new `ArgBuilder` of T from two `ArgBuilders` of `T` where the second `ArgBuilder` is a fallback if the first one fails
   * In the case that both fail, the error from the second will be returned
   * @param fallback The alternative `ArgBuilder` if this one fails
   */
  def orElse(fallback: ArgBuilder[T]): ArgBuilder[T] =
    (input: InputValue) =>
      self.build(input) match {
        case Left(_) => fallback.build(input)
        case pass    => pass
      }

  /**
   * @see [[orElse]]
   */
  def ||(fallback: ArgBuilder[T]): ArgBuilder[T] =
    orElse(fallback)
}

object ArgBuilder extends ArgBuilderInstances {
  def apply[T](implicit ev: ArgBuilder[T]): ArgBuilder[T] = ev

  object auto extends AutoArgBuilderDerivation
}

trait ArgBuilderInstances extends ArgBuilderDerivation {
  implicit lazy val unit: ArgBuilder[Unit]             = _ => Right(())
  implicit lazy val int: ArgBuilder[Int]               = {
    case value: IntValue => Right(value.toInt)
    case other           => Left(MkInvalidInputArgument("Int", other.toString))
  }
  implicit lazy val long: ArgBuilder[Long]             = {
    case value: IntValue    => Right(value.toLong)
    case StringValue(value) =>
      Try(value.toLong).fold(_ => Left(MkInvalidInputArgument("Long", value)), Right(_))
    case other              => Left(MkInvalidInputArgument("Long", other.toString))
  }
  implicit lazy val bigInt: ArgBuilder[BigInt]         = {
    case value: IntValue => Right(value.toBigInt)
    case other           => Left(MkInvalidInputArgument("BigInt", other.toString))
  }
  implicit lazy val float: ArgBuilder[Float]           = {
    case value: IntValue   => Right(value.toLong.toFloat)
    case value: FloatValue => Right(value.toFloat)
    case other             => Left(MkInvalidInputArgument("Float", other.toString))
  }
  implicit lazy val double: ArgBuilder[Double]         = {
    case value: IntValue   => Right(value.toLong.toDouble)
    case value: FloatValue => Right(value.toDouble)
    case other             => Left(MkInvalidInputArgument("Double", other.toString))
  }
  implicit lazy val bigDecimal: ArgBuilder[BigDecimal] = {
    case value: IntValue   => Right(BigDecimal(value.toBigInt))
    case value: FloatValue => Right(value.toBigDecimal)
    case other             => Left(MkInvalidInputArgument("BigDecimal", other.toString))
  }
  implicit lazy val string: ArgBuilder[String]         = {
    case StringValue(value) => Right(value)
    case other              => Left(MkInvalidInputArgument("String", other.toString))
  }
  implicit lazy val uuid: ArgBuilder[UUID]             = {
    case StringValue(value) =>
      Try(UUID.fromString(value)).fold(_ => Left(MkInvalidInputArgument("UUID", value)), Right(_))
    case other              => Left(MkInvalidInputArgument("UUID", other.toString))
  }
  implicit lazy val boolean: ArgBuilder[Boolean]       = {
    case BooleanValue(value) => Right(value)
    case other               => Left(MkInvalidInputArgument("Boolean", other.toString))
  }

  private abstract class TemporalDecoder[A](name: String) extends ArgBuilder[A] {
    protected[this] def parseUnsafe(input: String): A

    override def build(input: InputValue): Either[ExecutionError, A] = input match {
      case StringValue(value) =>
        try Right(parseUnsafe(value))
        catch {
          case NonFatal(e) =>
            val message = e.getMessage
            if (message.eq(null)) Left(MkInvalidInputArgument(name, value))
            else Left(MkInvalidInputArgument(name, s"$value ($message)"))
        }
      case _                  =>
        Left(MkInvalidInputArgument(name, input.toString))
    }
  }

  private object TemporalDecoder {
    def apply[T <: Temporal](name: String)(parse: String => T): TemporalDecoder[T] = new TemporalDecoder[T](name) {
      override protected[this] def parseUnsafe(input: String): T = parse(input)
    }
  }

  final def localDateWithFormatter(formatter: DateTimeFormatter): ArgBuilder[LocalDate]           =
    TemporalDecoder("LocalDate")(LocalDate.parse(_, formatter))
  final def localTimeWithFormatter(formatter: DateTimeFormatter): ArgBuilder[LocalTime]           =
    TemporalDecoder("LocalTime")(LocalTime.parse(_, formatter))
  final def localDateTimeWithFormatter(formatter: DateTimeFormatter): ArgBuilder[LocalDateTime]   =
    TemporalDecoder("LocalDateTime")(LocalDateTime.parse(_, formatter))
  final def offsetTimeWithFormatter(formatter: DateTimeFormatter): ArgBuilder[OffsetTime]         =
    TemporalDecoder("OffsetTime")(OffsetTime.parse(_, formatter))
  final def offsetDateTimeWithFormatter(formatter: DateTimeFormatter): ArgBuilder[OffsetDateTime] =
    TemporalDecoder("OffsetDateTime")(OffsetDateTime.parse(_, formatter))
  final def zonedDateTimeWithFormatter(formatter: DateTimeFormatter): ArgBuilder[ZonedDateTime]   =
    TemporalDecoder("ZonedDateTime")(ZonedDateTime.parse(_, formatter))

  lazy val instantEpoch: ArgBuilder[Instant] = {
    case i: IntValue => Right(Instant.ofEpochMilli(i.toLong))
    case value       => Left(MkInvalidInputArgument("Instant", value.toString))
  }

  implicit lazy val instant: ArgBuilder[Instant]               = TemporalDecoder("Instant")(Instant.parse)
  implicit lazy val localDate: ArgBuilder[LocalDate]           = TemporalDecoder("LocalDate")(LocalDate.parse)
  implicit lazy val localTime: ArgBuilder[LocalTime]           = TemporalDecoder("LocalTime")(LocalTime.parse)
  implicit lazy val localDateTime: ArgBuilder[LocalDateTime]   = TemporalDecoder("LocalDateTime")(LocalDateTime.parse)
  implicit lazy val offsetTime: ArgBuilder[OffsetTime]         = TemporalDecoder("OffsetTime")(OffsetTime.parse)
  implicit lazy val zonedDateTime: ArgBuilder[ZonedDateTime]   = TemporalDecoder("ZonedDateTime")(ZonedDateTime.parse)
  implicit lazy val offsetDateTime: ArgBuilder[OffsetDateTime] = TemporalDecoder("OffsetDateTime")(OffsetDateTime.parse)

  implicit def option[A](implicit ev: ArgBuilder[A]): ArgBuilder[Option[A]] = {
    case NullValue => Right(None)
    case value     => ev.build(value).map(Some(_))
  }
  implicit def list[A](implicit ev: ArgBuilder[A]): ArgBuilder[List[A]]     = {
    case InputValue.ListValue(items) =>
      items
        .foldLeft[Either[ExecutionError, List[A]]](Right(Nil)) {
          case (res @ Left(_), _)  => res
          case (Right(res), value) =>
            ev.build(value) match {
              case Left(error)  => Left(error)
              case Right(value) => Right(value :: res)
            }
        }
        .map(_.reverse)
    case other                       => ev.build(other).map(List(_))
  }

  implicit def seq[A](implicit ev: ArgBuilder[A]): ArgBuilder[Seq[A]]       = list[A].map(_.toSeq)
  implicit def set[A](implicit ev: ArgBuilder[A]): ArgBuilder[Set[A]]       = list[A].map(_.toSet)
  implicit def vector[A](implicit ev: ArgBuilder[A]): ArgBuilder[Vector[A]] = list[A].map(_.toVector)
  implicit def chunk[A](implicit ev: ArgBuilder[A]): ArgBuilder[Chunk[A]]   = list[A].map(Chunk.fromIterable)

  implicit lazy val upload: ArgBuilder[Upload] = {
    case StringValue(v) => Right(Upload(v))
    case other          => Left(MkInvalidInputArgument("Upload", other.toString))
  }
}

case object InvalidInputArgument extends NoStackTrace {
  override def getMessage: String = "invalid input argument"
}

private object MkInvalidInputArgument {
  private val vowels = BitSet(List('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U').map(_.toInt): _*)

  def apply(expected: String, actual: String): ExecutionError = {
    val prefix = if (expected.nonEmpty && vowels.contains(expected.codePointAt(0))) "an" else "a"
    ExecutionError(s"Can't build $prefix $expected from $actual", innerThrowable = Some(InvalidInputArgument))
  }
}
