package caliban.schema

import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime }
import java.util.UUID

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.util.Try
import scala.util.control.NonFatal
import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value._
import caliban.schema.Annotations.GQLName
import magnolia._
import mercator.Monadic
import zio.Chunk

/**
 * Typeclass that defines how to build an argument of type `T` from an input [[caliban.InputValue]].
 * Every type that can be passed as an argument needs an instance of `ArgBuilder`.
 */
@implicitNotFound(
  """Cannot find an ArgBuilder for type ${T}.
     
Caliban derives an ArgBuilder automatically for basic Scala types, case classes and sealed traits, but
you need to manually provide an implicit ArgBuilder for other types that could be nested in ${T}.
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

object ArgBuilder {

  type Typeclass[T] = ArgBuilder[T]

  implicit lazy val unit: ArgBuilder[Unit]             = _ => Right(())
  implicit lazy val int: ArgBuilder[Int]               = {
    case value: IntValue => Right(value.toInt)
    case other           => Left(ExecutionError(s"Can't build an Int from input $other"))
  }
  implicit lazy val long: ArgBuilder[Long]             = {
    case value: IntValue => Right(value.toLong)
    case other           => Left(ExecutionError(s"Can't build a Long from input $other"))
  }
  implicit lazy val bigInt: ArgBuilder[BigInt]         = {
    case value: IntValue => Right(value.toBigInt)
    case other           => Left(ExecutionError(s"Can't build a BigInt from input $other"))
  }
  implicit lazy val float: ArgBuilder[Float]           = {
    case value: IntValue   => Right(value.toLong.toFloat)
    case value: FloatValue => Right(value.toFloat)
    case other             => Left(ExecutionError(s"Can't build a Float from input $other"))
  }
  implicit lazy val double: ArgBuilder[Double]         = {
    case value: IntValue   => Right(value.toLong.toDouble)
    case value: FloatValue => Right(value.toDouble)
    case other             => Left(ExecutionError(s"Can't build a Double from input $other"))
  }
  implicit lazy val bigDecimal: ArgBuilder[BigDecimal] = {
    case value: IntValue   => Right(BigDecimal(value.toBigInt))
    case value: FloatValue => Right(value.toBigDecimal)
    case other             => Left(ExecutionError(s"Can't build a BigDecimal from input $other"))
  }
  implicit lazy val string: ArgBuilder[String]         = {
    case StringValue(value) => Right(value)
    case other              => Left(ExecutionError(s"Can't build a String from input $other"))
  }
  implicit lazy val uuid: ArgBuilder[UUID]             = {
    case StringValue(value) =>
      Try(UUID.fromString(value))
        .fold(ex => Left(ExecutionError(s"Can't parse $value into a UUID", innerThrowable = Some(ex))), Right(_))
    case other              => Left(ExecutionError(s"Can't build a UUID from input $other"))
  }
  implicit lazy val boolean: ArgBuilder[Boolean]       = {
    case BooleanValue(value) => Right(value)
    case other               => Left(ExecutionError(s"Can't build a Boolean from input $other"))
  }

  private abstract class TemporalDecoder[A](name: String) extends ArgBuilder[A] {
    protected[this] def parseUnsafe(input: String): A

    override def build(input: InputValue): Either[ExecutionError, A] = input match {
      case StringValue(value) =>
        try Right(parseUnsafe(value))
        catch {
          case NonFatal(e) =>
            val message = e.getMessage
            if (message.eq(null)) Left(ExecutionError(s"Can't build a $name from $value", innerThrowable = Some(e)))
            else Left(ExecutionError(s"Can't build a $name from $value ($message)", innerThrowable = Some(e)))
        }
      case _                  =>
        Left(ExecutionError(s"Can't build a $name from $input"))
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
    case value       => Left(ExecutionError(s"Can't build an Instant from $value"))
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

  type EitherExecutionError[A] = Either[ExecutionError, A]

  implicit val eitherMonadic: Monadic[EitherExecutionError] = new Monadic[EitherExecutionError] {
    override def flatMap[A, B](from: EitherExecutionError[A])(
      fn: A => EitherExecutionError[B]
    ): EitherExecutionError[B] = from.flatMap(fn)

    override def point[A](value: A): EitherExecutionError[A] = Right(value)

    override def map[A, B](from: EitherExecutionError[A])(fn: A => B): EitherExecutionError[B] = from.map(fn)
  }

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: InputValue) => {
      ctx.constructMonadic { p =>
        input match {
          case InputValue.ObjectValue(fields) =>
            val label = p.annotations.collectFirst { case GQLName(name) => name }.getOrElse(p.label)
            p.typeclass.build(fields.getOrElse(label, NullValue))
          case value                          => p.typeclass.build(value)
        }
      }
    }

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] = input => {
    (input match {
      case EnumValue(value)   => Some(value)
      case StringValue(value) => Some(value)
      case _                  => None
    }) match {
      case Some(value) =>
        ctx.subtypes
          .find(t =>
            t.annotations.collectFirst { case GQLName(name) => name }.contains(value) || t.typeName.short == value
          ) match {
          case Some(subtype) => subtype.typeclass.build(InputValue.ObjectValue(Map()))
          case None          => Left(ExecutionError(s"Invalid value $value for trait ${ctx.typeName.short}"))
        }
      case None        => Left(ExecutionError(s"Can't build a trait from input $input"))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
