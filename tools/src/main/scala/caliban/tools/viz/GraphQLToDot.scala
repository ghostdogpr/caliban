package caliban.tools.viz

import caliban.parsing.Parser
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.{ Definition, Directive, Directives, Document }
import caliban.schema.Schema
import caliban.tools.SchemaLoader
import zio.{ Task, ZIO }

object GraphQLToDot extends DotInstanceSyntax {

  def generate(document: Document): String = {
    val objects    = document.objectTypeDefinitions.map(_.toDot())
    val interfaces = document.interfaceTypeDefinitions.map(_.toDot())
    val inputs     = document.inputObjectTypeDefinitions.map(_.toDot())
    val enums      = document.enumTypeDefinitions.map(_.toDot())
    val unions     = document.unionTypeDefinitions
    val relations  = Relations.fromTypes(document.typeDefinitions).map(_.toDot())
    val dotfile    = s"""
                     |digraph erd {
                     |  rankdir = "LR";
                     |  node [
                     |    fontsize = "16"
                     |    shape = "plaintext"
                     |  ];
                     |  edge [
                     |  ];
                     |${Rendering.withIndent(2)(objects.mkString("\n"))}
                     |${Rendering.withIndent(2)(interfaces.mkString("\n"))}
                     |${Rendering.withIndent(2)(inputs.mkString("\n"))}
                     |${Rendering.withIndent(2)(enums.mkString("\n"))}
                     |${Rendering.withIndent(2)(relations.mkString("\n"))}
                     |}
                     |""".stripMargin.trim()
    dotfile + "\n\n"
  }
}
