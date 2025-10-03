package caliban.codegen.sbt

import _root_.sbt.*

trait CompileTimeCalibanClientPluginCompat {
  extension [A](init: Def.Initialize[Task[A]]) {
    final def maybeTaskValue: Def.Initialize[Task[A]] = init
  }
}
