package caliban.derivation.annotation

import caliban.schema.Schema

import scala.language.experimental.macros
import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.reflect.macros.whitebox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class deriveSchema extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DeriveSchemaImpl.impl
}

class DeriveSchemaImpl(val c: whitebox.Context) {
  import c.universe._

  private val macroPath: Tree = q"_root_.caliban.derivation.deriveSchemaInstance"

  private val schemaSym: Symbol = symbolOf[Schema[_, _]]

  private def invoke(cd: ClassDef): Tree = {
    val methodName = TermName(c.freshName())

    if (cd.tparams.isEmpty) {
      val tpe = tq"${cd.name}"

      q"implicit val $methodName: $schemaSym[Any, $tpe] = $macroPath[$tpe]"
    } else {
      val tparams = cd.tparams.map(_.duplicate)
      val targs   = tparams.map(tp => tq"${tp.name}")

      val tpe = tq"${cd.name}[..$targs]"

      q"implicit def $methodName[..$tparams]: $schemaSym[Any, $tpe] = $macroPath[$tpe]"
    }
  }

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val inputs: List[Tree] = annottees.map(_.tree).toList

    val expandees: List[Tree] =
      inputs match {
        case List(cd: ClassDef) =>
          val md =
            q"""
              object ${cd.name.toTermName} {
                ${invoke(cd)}
              }
            """
          List(cd, md)

        case List(cd: ClassDef, md: ModuleDef) =>
          val ModuleDef(mods, name, Template(parents, self, body)) = md

          val companion =
            ModuleDef(mods, name, Template(parents, self, body :+ invoke(cd)))

          List(cd, companion)

        case _ =>
          c.abort(c.enclosingPosition, s"deriveSchema only supports class annottees")
      }

    c.Expr(Block(expandees, Literal(Constant(()))))
  }
}
