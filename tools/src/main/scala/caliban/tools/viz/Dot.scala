package caliban.tools.viz
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ AggregationTypeDefinition, TypeDefinition }
import caliban.parsing.adt.{ Definition, Directive, Directives, Document, Type }

sealed trait DotEdge {
  def toDot(): String
}
object DotEdge       {
  sealed trait FieldRelation extends DotEdge
  sealed trait TypeRelation  extends DotEdge
  sealed trait InputRelation extends DotEdge
  object FieldRelation {
    private case class FieldRelationImpl(fromType: String, fromField: String, toType: String) extends FieldRelation {
      def toDot(): String = s""""${fromType}":${fromField}port -> "${toType}";"""
    }
    def apply(fromType: String, fromField: String, toType: String): FieldRelation =
      FieldRelationImpl(fromType, fromField, toType)
    def unapply(relation: DotEdge): Option[(String, String, String)] = relation match {
      case FieldRelationImpl(fromType, fromField, toType) => Some((fromType, fromField, toType))
      case _                                              => None
    }
  }
  object TypeRelation  {
    private case class TypeRelationImpl(fromType: String, toType: String, label: Option[String] = None)
        extends TypeRelation {
      def toDot(): String = s""""${fromType}":__title -> "${toType}":__title;"""
    }
    def apply(fromType: String, toType: String, label: Option[String] = None): TypeRelation =
      TypeRelationImpl(fromType, toType, label)
    def unapply(relation: DotEdge): Option[(String, String, Option[String])] = relation match {
      case TypeRelationImpl(fromType, toType, label) => Some((fromType, toType, label))
      case _                                         => None
    }
  }
  object InputRelation {
    private case class InputRelationImpl(fromType: String, toType: String, toField: String) extends InputRelation {
      def toDot(): String = s""""${fromType}" -> "${toType}":${toField}port;"""
    }
    def apply(fromType: String, toType: String, toField: String): InputRelation =
      InputRelationImpl(fromType, toType, toField)
    def unapply(relation: DotEdge): Option[(String, String, String)] = relation match {
      case InputRelationImpl(fromType, toType, toField) => Some((fromType, toType, toField))
      case _                                            => None
    }
  }
}

object Relations {
  def fromTypes(types: List[TypeDefinition]): List[DotEdge] = {
    val knownTypeNames = types.map(_.name).toSet
    types.flatMap {
      case obj: ObjectTypeDefinition  =>
        val implRels  = obj.implements.flatMap { impl =>
          val tpe = impl.name
          if (knownTypeNames.contains(tpe))
            DotEdge.TypeRelation(obj.name, impl.name) :: Nil
          else Nil
        }
        val fieldRels = obj.fields.flatMap { field =>
          val tpe                         = Type.innerType(field.ofType)
          val fieldTypeRel                =
            if (knownTypeNames.contains(tpe))
              DotEdge.FieldRelation(obj.name, field.name, tpe) :: Nil
            else Nil
          val fieldArgRels: List[DotEdge] = field.args.flatMap { arg =>
            val argTpe = Type.innerType(arg.ofType)
            if (knownTypeNames.contains(argTpe))
              DotEdge.InputRelation(argTpe, obj.name, field.name) :: Nil
            else Nil
          }
          fieldTypeRel ::: fieldArgRels
        }
        fieldRels ::: implRels
      case union: UnionTypeDefinition =>
        union.memberTypes.flatMap { name =>
          if (knownTypeNames.contains(name))
            Some(DotEdge.TypeRelation(union.name, name))
          else
            None
        }
      case _                          => Nil
    }
  }
}

trait DotNode[T <: TypeDefinition] {
  def toDot(t: T)(theme: Theme = Theme.default): String
  def stereotype: Option[String]
  protected def groupLabel: String
  protected def stereotypeLabel: String = stereotype match {
    case Some(name) if name.nonEmpty => s"&laquo;$name&raquo;<BR/>"
    case _                           => ""
  }

  protected def tableHead(t: T)(theme: Theme) =
    s"""
       |<TR>
       |  <TD PORT="__title"><FONT COLOR="${theme.color}">$stereotypeLabel<B>${t.name}</B></FONT></TD>
       |</TR>
       |""".stripMargin.trim()
}

trait AggDotNode[T <: AggregationTypeDefinition] extends DotNode[T] {
  override def toDot(t: T)(theme: Theme = Theme.default): String =
    s"""
       |"${t.name}" [
       |  label=<
       |    <TABLE
       |      COLOR="${theme.color}"
       |      BORDER="0"
       |      CELLBORDER="1"
       |      CELLSPACING="0"
       |    >
       |${Rendering.withIndent(4)(tableHead(t)(theme))}
       |${Rendering.withIndent(4)(t.fields.map(Fields.field(_)(theme)).mkString("\n"))}
       |    </TABLE>
       |  >
       |]""".stripMargin.trim()
}

object DotNode {
  implicit val enumToDot: DotNode[EnumTypeDefinition]               = new DotNode[EnumTypeDefinition] {
    def stereotype: Option[String]                                         = Some("enumeration")
    def groupLabel: String                                                 = "Enum Types"
    def toDot(t: EnumTypeDefinition)(theme: Theme = Theme.default): String =
      s"""
         |"${t.name}" [
         |  label=<
         |    <TABLE
         |      COLOR="${theme.color}"
         |      BORDER="0"
         |      CELLBORDER="1"
         |      CELLSPACING="0"
         |    >
         |${Rendering.withIndent(4)(tableHead(t)(theme))}
         |${Rendering.withIndent(4)(t.enumValuesDefinition.map(Fields.field(_)(theme)).mkString("\n"))}
         |    </TABLE>
         |  >
         |]""".stripMargin.trim()
  }
  implicit val inputObjectToDot: DotNode[InputObjectTypeDefinition] = new DotNode[InputObjectTypeDefinition] {
    def stereotype: Option[String]                                                = Some("input")
    def groupLabel: String                                                        = "Input Types"
    def toDot(t: InputObjectTypeDefinition)(theme: Theme = Theme.default): String =
      s"""
         |"${t.name}" [
         |  label=<
         |    <TABLE
         |      COLOR="${theme.color}"
         |      BORDER="0"
         |      CELLBORDER="1"
         |      CELLSPACING="0"
         |    >
         |${Rendering.withIndent(4)(tableHead(t)(theme))}
         |${Rendering.withIndent(4)(t.fields.map(Fields.field(_)(theme)).mkString("\n"))}
         |    </TABLE>
         |  >
         |]""".stripMargin.trim()
  }
  implicit val objToDot: DotNode[ObjectTypeDefinition]              = new AggDotNode[ObjectTypeDefinition] {
    def stereotype: Option[String] = None
    def groupLabel: String         = "Types"
  }
  implicit val interfaceToDot: DotNode[InterfaceTypeDefinition]     = new AggDotNode[InterfaceTypeDefinition] {
    def stereotype: Option[String] = Some("interface")
    def groupLabel: String         = "Interface Types"
  }
}

private[viz] object Fields {
  def field(f: EnumValueDefinition)(theme: Theme): String  =
    field(f.enumValue, fieldLabel(f)(theme), theme)
  def field(f: InputValueDefinition)(theme: Theme): String =
    field(f.name, fieldLabel(f)(theme), theme)

  def field(f: FieldDefinition)(theme: Theme): String =
    field(f.name, fieldLabel(f)(theme), theme)

  def field(name: String, label: String, theme: Theme): String =
    s"""
       |<TR>
       |  <TD ALIGN="${theme.align}" PORT="${name}port"><FONT COLOR="${theme.color}">$label</FONT></TD>
       |</TR>
       |""".stripMargin.trim()

  def fieldLabel(f: EnumValueDefinition)(theme: Theme): String  = Fields.notes(f.directives) match {
    case None        => f.enumValue
    case Some(notes) => s"""${f.enumValue} $notes"""
  }
  def fieldLabel(f: InputValueDefinition)(theme: Theme): String = {
    val name  = f.name
    val tpe   = f.ofType.toString()
    val notes = Fields.notes(f.directives)
    val field = s"${name}: ${tpe}"
    notes match {
      case Some(notes) => s"$field $notes"
      case None        => field
    }
  }
  def fieldLabel(f: FieldDefinition)(theme: Theme): String      = {
    val name  = f.name
    val args  = Fields.args(f.args)
    val tpe   = f.ofType.toString()
    val notes = Fields.notes(f.directives)
    val field = s"${name}${args}: ${tpe}"
    notes match {
      case Some(notes) => s"$field $notes"
      case None        => field
    }
  }
  def args(args: List[InputValueDefinition]): String            = args match {
    case Nil  => ""
    case args => args.map(arg => s"""${arg.name}: ${arg.ofType.toString()}""").mkString("(", ", ", ")")
  }

  def notes(directives: List[Directive]): Option[String] = {
    val deprecation = (Directives.isDeprecated(directives), Directives.deprecationReason(directives)) match {
      case (true, Some(reason)) => Some(reason)
      case (true, None)         => Some("Deprecated")
      case _                    => None
    }
    deprecation match {
      case None       => None
      case Some(text) => Some(s"""<FONT COLOR="RED">$text</FONT>""")
    }
  }
}

private[viz] trait DotInstanceSyntax {
  implicit class DotOps[T <: TypeDefinition](t: T)(implicit dot: DotNode[T]) {
    def toDot(theme: Theme = Theme.default): String = dot.toDot(t)(theme)
  }
}

private[viz] object Rendering {
  def withIndent(level: Int)(lines: String): String = {
    val indent = " " * level * 2
    lines.split("\n").map(indent + _).mkString("\n")
  }
}
