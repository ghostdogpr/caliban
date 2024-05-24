package caliban

import caliban.schema.ArgBuilder
import caliban.schema.Schema
import caliban.schema.Step
import caliban.schema.Step.PureStep

sealed trait TriState[+A]
object TriState {
  case object Undefined     extends TriState[Nothing]
  case object Null          extends TriState[Nothing]
  case class Value[A](v: A) extends TriState[A]

  def fromOption[A](o: Option[A]) = o.fold[TriState[A]](Null)(Value(_))

  def schemaCustom[R, A](undefined: PureStep)(implicit ev: Schema[R, A]): Schema[R, TriState[A]] =
    new Schema[R, TriState[A]] {
      override val nullable = true

      override def toType(isInput: Boolean, isSubscription: Boolean) = ev.toType_(isInput, isSubscription)

      override def resolve(value: TriState[A]) =
        value match {
          case Undefined => undefined
          case Null      => Step.NullStep
          case Value(v)  => ev.resolve(v)
        }
    }

  implicit def schema[R, A](implicit ev: Schema[R, A]): Schema[R, TriState[A]] = schemaCustom(Step.NullStep)

  implicit def argBuilder[A](implicit ev: ArgBuilder[A]): ArgBuilder[TriState[A]] = new ArgBuilder[TriState[A]] {
    private val base = ArgBuilder.option(ev)

    override def build(input: InputValue) = base.build(input).map(fromOption(_))

    override def buildMissing(default: Option[String]) = default match {
      case None    => Right(Undefined)
      case Some(v) => base.buildMissing(Some(v)).map(fromOption(_))
    }
  }
}
