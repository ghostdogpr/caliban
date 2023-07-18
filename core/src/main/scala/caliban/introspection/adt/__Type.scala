package caliban.introspection.adt

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Directive, Type }

case class __Type(
  kind: __TypeKind,
  name: Option[String] = None,
  description: Option[String] = None,
  fields: __DeprecatedArgs => Option[List[__Field]] = _ => None,
  interfaces: () => Option[List[__Type]] = () => None,
  possibleTypes: Option[List[__Type]] = None,
  enumValues: __DeprecatedArgs => Option[List[__EnumValue]] = _ => None,
  inputFields: Option[List[__InputValue]] = None,
  ofType: Option[__Type] = None,
  specifiedBy: Option[String] = None,
  directives: Option[List[Directive]] = None,
  origin: Option[String] = None
) { self =>
  def |+|(that: __Type): __Type = __Type(
    kind,
    (name ++ that.name).reduceOption((_, b) => b),
    (description ++ that.description).reduceOption((_, b) => b),
    args => (fields(args) ++ that.fields(args)).reduceOption(_ ++ _),
    () => (interfaces() ++ that.interfaces()).reduceOption(_ ++ _),
    (possibleTypes ++ that.possibleTypes).reduceOption(_ ++ _),
    args => (enumValues(args) ++ that.enumValues(args)).reduceOption(_ ++ _),
    (inputFields ++ that.inputFields).reduceOption(_ ++ _),
    (ofType ++ that.ofType).reduceOption(_ |+| _),
    (specifiedBy ++ that.specifiedBy).reduceOption((_, b) => b),
    (directives ++ that.directives).reduceOption(_ ++ _),
    (origin ++ that.origin).reduceOption((_, b) => b)
  )

  def toType(nonNull: Boolean = false): Type =
    ofType match {
      case Some(of) =>
        kind match {
          case __TypeKind.LIST     => ListType(of.toType(), nonNull)
          case __TypeKind.NON_NULL => of.toType(true)
          case _                   => NamedType(name.getOrElse(""), nonNull)
        }
      case None     => NamedType(name.getOrElse(""), nonNull)
    }

  def toTypeDefinition: Option[TypeDefinition] =
    kind match {
      case __TypeKind.SCALAR       =>
        Some(
          ScalarTypeDefinition(
            description,
            name.getOrElse(""),
            directives
              .getOrElse(Nil) ++
              specifiedBy
                .map(url => Directive("specifiedBy", Map("url" -> StringValue(url)), directives.size))
                .toList
          )
        )
      case __TypeKind.OBJECT       =>
        Some(
          ObjectTypeDefinition(
            description,
            name.getOrElse(""),
            interfaces().getOrElse(Nil).map(t => NamedType(t.name.getOrElse(""), nonNull = false)),
            directives.getOrElse(Nil),
            allFields.map(_.toFieldDefinition)
          )
        )
      case __TypeKind.INTERFACE    =>
        Some(
          InterfaceTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            allFields.map(_.toFieldDefinition)
          )
        )
      case __TypeKind.UNION        =>
        Some(
          UnionTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            possibleTypes.getOrElse(Nil).flatMap(_.name)
          )
        )
      case __TypeKind.ENUM         =>
        Some(
          EnumTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            enumValues(__DeprecatedArgs(Some(true))).getOrElse(Nil).map(_.toEnumValueDefinition)
          )
        )
      case __TypeKind.INPUT_OBJECT =>
        Some(
          InputObjectTypeDefinition(
            description,
            name.getOrElse(""),
            directives.getOrElse(Nil),
            inputFields.getOrElse(Nil).map(_.toInputValueDefinition)
          )
        )
      case _                       => None
    }

  def isNullable: Boolean =
    kind match {
      case __TypeKind.NON_NULL => false
      case _                   => true
    }

  def list: __Type    = __Type(__TypeKind.LIST, ofType = Some(self))
  def nonNull: __Type = __Type(__TypeKind.NON_NULL, ofType = Some(self))

  lazy val allFields: List[__Field] =
    fields(__DeprecatedArgs(Some(true))).getOrElse(Nil)
}

sealed trait TypeVisitor { self =>
  import TypeVisitor._

  def |+|(that: TypeVisitor): TypeVisitor = TypeVisitor.Combine(self, that)

  def visit(t: __Type): __Type = {
    def collect(visitor: TypeVisitor): __Type => __Type =
      visitor match {
        case Modify(f)       => f
        case Combine(v1, v2) => collect(v1) andThen collect(v2)
      }

    val f = collect(self)

    def loop(t: __Type): __Type =
      f(
        t.copy(
          fields = args => t.fields(args).map(_.map(field => field.copy(`type` = () => loop(field.`type`())))),
          inputFields = t.inputFields.map(_.map(field => field.copy(`type` = () => loop(field.`type`())))),
          interfaces = () => t.interfaces().map(_.map(loop)),
          possibleTypes = t.possibleTypes.map(_.map(loop)),
          ofType = t.ofType.map(loop)
        )
      )

    loop(t)
  }
}

object TypeVisitor {
  private case class Modify(f: __Type => __Type)               extends TypeVisitor
  private case class Combine(v1: TypeVisitor, v2: TypeVisitor) extends TypeVisitor

  def modify(f: __Type => __Type): TypeVisitor          = Modify(f)
  def modify[A](visitor: OptionVisitor[A]): TypeVisitor = modify(t => visitor.visit(t))
  def modify[A](visitor: ListVisitor[A]): TypeVisitor   = modify(t => visitor.visit(t))
}

sealed abstract class OptionVisitor[A](implicit val set: __Type => (Option[A] => Option[A]) => __Type) { self =>
  import OptionVisitor._

  def |+|(that: OptionVisitor[A]): OptionVisitor[A] = OptionVisitor.Combine(self, that)(set)

  def visit(t: __Type): __Type =
    self match {
      case Modify(f)       => set(t)(_.map(f(t)))
      case Combine(v1, v2) => v2.visit(v1.visit(t))
    }
}

object OptionVisitor {
  private case class Modify[A](f: __Type => A => A)(implicit
    set: __Type => (Option[A] => Option[A]) => __Type
  ) extends OptionVisitor[A]
  private case class Combine[A](v1: OptionVisitor[A], v2: OptionVisitor[A])(implicit
    set: __Type => (Option[A] => Option[A]) => __Type
  ) extends OptionVisitor[A]

  def modify[A](f: (__Type, A) => A)(implicit set: __Type => (Option[A] => Option[A]) => __Type): OptionVisitor[A] =
    Modify(t => field => f(t, field))
}

sealed abstract class ListVisitor[A](implicit val set: __Type => (List[A] => List[A]) => __Type) { self =>
  import ListVisitor._

  def |+|(that: ListVisitor[A]): ListVisitor[A] = ListVisitor.Combine(self, that)(set)

  def visit(t: __Type): __Type =
    self match {
      case Filter(predicate) => set(t)(_.filter(predicate(t)))
      case Modify(f)         => set(t)(_.map(f(t)))
      case Add(f)            => set(t)(f(t).foldLeft(_) { case (as, a) => a :: as })
      case Combine(v1, v2)   => v2.visit(v1.visit(t))
    }
}

trait OptionVisitorConstructors[A] {
  implicit val set: __Type => (Option[A] => Option[A]) => __Type

  def modify(f: A => A): OptionVisitor[A]               = modifyWith((_, a) => f(a))
  def modifyWith(f: (__Type, A) => A): OptionVisitor[A] = OptionVisitor.modify(f)
}

object NameVisitor extends OptionVisitorConstructors[String] {
  val set: __Type => (Option[String] => Option[String]) => __Type =
    t => f => t.copy(name = f(t.name))
}

object ListVisitor {
  private case class Filter[A](predicate: __Type => A => Boolean)(implicit
    set: __Type => (List[A] => List[A]) => __Type
  ) extends ListVisitor[A]
  private case class Modify[A](f: __Type => A => A)(implicit
    set: __Type => (List[A] => List[A]) => __Type
  ) extends ListVisitor[A]
  private case class Add[A](f: __Type => List[A])(implicit
    set: __Type => (List[A] => List[A]) => __Type
  ) extends ListVisitor[A]
  private case class Combine[A](v1: ListVisitor[A], v2: ListVisitor[A])(implicit
    set: __Type => (List[A] => List[A]) => __Type
  ) extends ListVisitor[A]

  def filter[A](predicate: (__Type, A) => Boolean)(implicit
    set: __Type => (List[A] => List[A]) => __Type
  ): ListVisitor[A] = Filter(t => field => predicate(t, field))
  def modify[A](f: (__Type, A) => A)(implicit set: __Type => (List[A] => List[A]) => __Type): ListVisitor[A] =
    Modify(t => field => f(t, field))
  def add[A](f: __Type => List[A])(implicit set: __Type => (List[A] => List[A]) => __Type): ListVisitor[A]   = Add(f)
}

trait ListVisitorConstructors[A] {
  implicit val set: __Type => (List[A] => List[A]) => __Type

  def filter(predicate: A => Boolean): ListVisitor[A]               = filterWith((_, a) => predicate(a))
  def filterWith(predicate: (__Type, A) => Boolean): ListVisitor[A] = ListVisitor.filter(predicate)
  def modify(f: A => A): ListVisitor[A]                             = modifyWith((_, a) => f(a))
  def modifyWith(f: (__Type, A) => A): ListVisitor[A]               = ListVisitor.modify(f)
  def add(list: List[A]): ListVisitor[A]                            = addWith(_ => list)
  def addWith(f: __Type => List[A]): ListVisitor[A]                 = ListVisitor.add(f)
}

object FieldVisitor extends ListVisitorConstructors[__Field] {
  val set: __Type => (List[__Field] => List[__Field]) => __Type =
    t => f => t.copy(fields = args => t.fields(args).map(f))
}

object InputFieldVisitor extends ListVisitorConstructors[__InputValue] {
  val set: __Type => (List[__InputValue] => List[__InputValue]) => __Type =
    t => f => t.copy(inputFields = t.inputFields.map(f))
}

object EnumValueVisitor extends ListVisitorConstructors[__EnumValue] {
  val set: __Type => (List[__EnumValue] => List[__EnumValue]) => __Type =
    t => f => t.copy(enumValues = args => t.enumValues(args).map(f))
}
