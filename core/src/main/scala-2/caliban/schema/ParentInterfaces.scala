package caliban.schema

import scala.language.experimental.macros

final class ParentInterfaces[R, T](val schemas: () => List[Schema[R, _]])

object ParentInterfaces {
  implicit def materialize[R, T]: ParentInterfaces[R, T] = macro ParentInterfacesMacro.materialize[R, T]
}
