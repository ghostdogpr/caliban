package caliban.modelgen

import caliban.parsing.adt.{Document, Type}
import caliban.parsing.adt.Document._
import caliban.parsing.adt.ExecutableDefinition.TypeDefinition
import caliban.parsing.adt.Type.{FieldDefinition, ListType, NamedType}
import zio.IO

object Generator {
  def generate(doc: Document): String =
  s"""
  object Types {
    ${typeDefinitions(doc).map(caseClassFromType(_)).mkString("\n\n")}
  }

  object Queries {

  }

  object Mutations {

  }

  object Subscriptions {

  }

  object Fragments {

  }
  """



  def definitions[T](doc: Document): List[T] =
    doc.definitions.flatMap {
      case t: T => List(t)
      case _                 => List()
    }


  def caseClassFromType(t: TypeDefinition): String =
    s"""case class ${t.name}(${t.children.map(fieldToParameter(_)).mkString(", ")})"""

  def fieldToParameter(field: FieldDefinition): String =
    s"""${field.name}: ${gqlTypeToScalaType(field.ofType)}"""

  def gqlTypeToScalaType(t: Type): String = t match {
    case NamedType(name, nonNull) if (nonNull)   => typeByName(name)
    case NamedType(name, nonNull) if (!nonNull)  => s"Option[${typeByName(name)}]"
    case ListType(ofType, nonNull) if (nonNull)  => s"List[${gqlTypeToScalaType(ofType)}]"
    case ListType(ofType, nonNull) if (!nonNull) => s"Option[List[${gqlTypeToScalaType(ofType)}]]"
  }

  def typeByName(name: String): String = name match {
    case _ => name
  }



  //gen fields

}
