package caliban.codegen.sbt

import _root_.sbt._
import _root_.sbt.std.InputWrapper
import scala.language.experimental.macros

trait CompileTimeCalibanClientPluginCompat {
  implicit class DefInitializeTaskSyntax[A](init: Def.Initialize[Task[A]]) {
    final def flatMapTask[B](f: A => Task[B]): Def.Initialize[Task[B]] = init.flatMap(f)

    final def maybeTaskValue: Task[A] = macro InputWrapper.taskValueMacroImpl[A]
  }
}
