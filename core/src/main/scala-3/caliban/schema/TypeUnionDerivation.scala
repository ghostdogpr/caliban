package caliban.schema

import caliban.introspection.adt.__Type

import scala.quoted.*

object TypeUnionDerivation {
  inline def derived[R, T]: Schema[R, T] = ${ typeUnionSchema[R, T] }

  def typeUnionSchema[R: Type, T: Type](using quotes: Quotes): Expr[Schema[R, T]] = {
    import quotes.reflect.*

    class TypeAndSchema[A](val typeRef: String, val schema: Expr[Schema[R, A]], val tpe: Type[A])

    def rec[A](using tpe: Type[A]): List[TypeAndSchema[?]] =
      TypeRepr.of(using tpe).dealias match {
        case OrType(l, r) =>
          rec(using l.asType.asInstanceOf[Type[Any]]) ++ rec(using r.asType.asInstanceOf[Type[Any]])
        case otherRepr    =>
          val otherString: String    = otherRepr.show
          val expr: TypeAndSchema[A] =
            Expr.summon[Schema[R, A]] match {
              case Some(foundSchema) =>
                TypeAndSchema[A](otherString, foundSchema, otherRepr.asType.asInstanceOf[Type[A]])
              case None              =>
                quotes.reflect.report.errorAndAbort(s"Couldn't resolve Schema[Any, $otherString]")
            }

          List(expr)
      }

    val typeAndSchemas: List[TypeAndSchema[?]] = rec[T]

    val schemaByTypeNameList: Expr[List[(String, Schema[R, Any])]] = Expr.ofList(
      typeAndSchemas.map { case (tas: TypeAndSchema[a]) =>
        given Type[a] = tas.tpe
        '{ (${ Expr(tas.typeRef) }, ${ tas.schema }.asInstanceOf[Schema[R, Any]]) }
      }
    )
    val name                                                       = TypeRepr.of[T].show

    if (name.contains("|")) {
      report.error(
        s"You must explicitly add type parameter to derive Schema for a union type in order to capture the name of the type alias"
      )
    }

    '{
      val schemaByName: Map[String, Schema[R, Any]] = ${ schemaByTypeNameList }.toMap
      new Schema[R, T] {

        def resolve(value: T): Step[R] = {
          var ret: Step[R] = null
          ${
            Expr.block(
              typeAndSchemas.map { case (tas: TypeAndSchema[a]) =>
                given Type[a] = tas.tpe

                '{ if value.isInstanceOf[a] then ret = schemaByName(${ Expr(tas.typeRef) }).resolve(value) }
              },
              '{ require(ret != null, s"no schema for ${value}") }
            )
          }
          ret
        }

        def toType(isInput: Boolean, isSubscription: Boolean): __Type =
          Types.makeUnion(
            Some(${ Expr(name) }),
            None,
            schemaByName.values.map(_.toType_(isInput, isSubscription)).toList
          )
      }
    }
  }
}
