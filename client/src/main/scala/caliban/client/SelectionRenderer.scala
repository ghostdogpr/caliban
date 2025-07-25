package caliban.client

import caliban.client.Selection.Directive
import caliban.client.__Value.__NullValue

import scala.annotation.tailrec

trait SelectionRenderer[-A, S] { self =>
  type State = S

  def render(a: A, state0: State, options: SelectionRenderer.Options): (String, State) = {
    val sb     = new StringBuilder()
    val state1 = unsafeRender(a, state0, sb, options)
    (sb.toString, state1)
  }

  def unsafeRender(
    a: A,
    state: State,
    writer: StringBuilder,
    options: SelectionRenderer.Options
  ): State

}

object SelectionRenderer {
  case class Options(
    useVariables: Boolean,
    dropNullInputValues: Boolean,
    operationName: String,
    queryName: Option[String],
    indent: Option[Int]
  )

  case class SelectionState(
    variables: Map[String, (__Value, String)],
    fragments: Set[Selection.FragmentSpread]
  )

  def empty[S]: SelectionRenderer[Any, S] =
    Empty.asInstanceOf[SelectionRenderer[Any, S]]

  private case object Empty extends SelectionRenderer[Any, Any] {
    override def unsafeRender(a: Any, state: Any, writer: StringBuilder, options: Options): Any =
      state
  }

  lazy val requestRenderer: SelectionRenderer[List[Selection], Map[String, (__Value, String)]] =
    new SelectionRenderer[List[Selection], Map[String, (__Value, String)]] {
      override def unsafeRender(a: List[Selection], state: State, writer: StringBuilder, options: Options): State = {
        val state0 = SelectionRenderer.SelectionState(state, Set.empty)
        val inner  = new StringBuilder()

        writer.append(options.operationName)
        options.queryName.foreach { queryName =>
          writer.append(' ')
          writer.append(queryName)
          writer.append(' ')
        }
        val state1 = SelectionRenderer.selectionsRenderer.unsafeRender(a, state0, inner, options)
        val state2 = SelectionRenderer.fragmentsRenderer.unsafeRender(state1.fragments, state1, inner, options)

        SelectionRenderer.variablesRenderer.unsafeRender(state2.variables, (), writer, options)
        writer.append(inner)
        state2.variables
      }
    }

  def list[A, S](renderer: SelectionRenderer[A, S], separator: Char): SelectionRenderer[List[A], S] =
    new SelectionRenderer[List[A], S] {
      override def unsafeRender(a: List[A], state0: S, writer: StringBuilder, options: Options): S = {
        @tailrec
        def loop(remaining: List[A], state: S, first: Boolean): S = remaining match {
          case Nil          => state
          case head :: tail =>
            if (!first) writer.append(separator)
            val s0 = renderer.unsafeRender(head, state, writer, options)
            loop(tail, s0, first = false)
        }

        loop(a, state0, first = true)
      }
    }

  def set[A, S](renderer: SelectionRenderer[A, S], separator: Char): SelectionRenderer[Set[A], S] =
    new SelectionRenderer[Set[A], S] {
      override def unsafeRender(a: Set[A], state0: S, writer: StringBuilder, options: Options): S = {
        @tailrec
        def loop(remaining: List[A], state: S, first: Boolean): S = remaining match {
          case Nil          => state
          case head :: tail =>
            if (!first) writer.append(separator)
            val s0 = renderer.unsafeRender(head, state, writer, options)
            loop(tail, s0, first = false)
        }

        loop(a.toList, state0, first = true)
      }
    }

  private lazy val variablesRenderer: SelectionRenderer[Map[String, (__Value, String)], Unit] =
    new SelectionRenderer[Map[String, (__Value, String)], Unit] {
      private val listRenderer: SelectionRenderer[List[(String, String)], Unit] =
        list(variableRenderer, ',')
      override def unsafeRender(
        a: Map[String, (__Value, String)],
        state: Unit,
        writer: StringBuilder,
        options: Options
      ): Unit =
        if (a.nonEmpty) {
          writer.append('(')
          listRenderer.unsafeRender(a.map(kv => kv._1 -> kv._2._2).toList, state, writer, options)
          writer.append(')')
        }
    }

  private lazy val variableRenderer: SelectionRenderer[(String, String), Unit] =
    new SelectionRenderer[(String, String), Unit] {
      override def unsafeRender(a: (String, String), state: Unit, writer: StringBuilder, options: Options): Unit = {
        writer.append('$')
        writer.append(a._1)
        writer.append(": ")
        writer.append(a._2)
        ()
      }
    }

  lazy val selectionsRenderer: SelectionRenderer[List[Selection], SelectionState] =
    new SelectionRenderer[List[Selection], SelectionState] {
      override def unsafeRender(
        a: List[Selection],
        state: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        @tailrec
        def loop(
          selections: List[Selection],
          state0: State,
          names: Set[String],
          first: Boolean = false
        ): State =
          selections match {
            case Nil                                    => state0
            case (f: Selection.Field) :: rest           =>
              if (!first) writer.append(' ')
              val hasAlias   = f.alias.isDefined
              val resolved   = f.alias.getOrElse(f.name)
              if (!names(resolved)) {
                writer.append(resolved)
                if (hasAlias) {
                  writer.append(':')
                  writer.append(f.name)
                }
              } else {
                writer.append(resolved)
                writer.append(math.abs(f.code))
                if (hasAlias) {
                  writer.append(':')
                  writer.append(f.name)
                }
              }
              val bodyWriter = new StringBuilder()
              var state1     = selectionsRenderer.unsafeRender(f.selectionSet, state0, bodyWriter, options)
              if (f.arguments.nonEmpty) {
                writer.append('(')
                state1 = argumentsRenderer.unsafeRender(f.arguments, state1, writer, options)
                writer.append(')')
              }
              if (f.directives.nonEmpty) {
                state1 = directivesRenderer.unsafeRender(f.directives, state1, writer, options)
              }
              writer.append(bodyWriter)
              loop(rest, state1, names + resolved)
            case (fs: Selection.FragmentSpread) :: rest =>
              if (!first) writer.append(' ')

              val withFragment = state.copy(fragments = state.fragments + fs)
              loop(
                rest,
                fragmentSpreadInlineRenderer.unsafeRender(fs, withFragment, writer, options),
                names
              )
            case (is: Selection.InlineFragment) :: rest =>
              if (!first) writer.append(' ')
              loop(
                rest,
                inlineFragmentRenderer.unsafeRender(is, state, writer, options),
                names
              )
          }

        if (a.isEmpty) state
        else {
          writer.append('{')
          val state1 = loop(a, state, Set.empty, first = true)
          writer.append('}')
          state1
        }
      }
    }

  private lazy val inlineFragmentRenderer: SelectionRenderer[Selection.InlineFragment, SelectionState] =
    new SelectionRenderer[Selection.InlineFragment, SelectionState] {
      override def unsafeRender(
        a: Selection.InlineFragment,
        state: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        writer.append("... on ")
        writer.append(a.onType)
        selectionsRenderer.unsafeRender(a.selectionSet, state, writer, options)
      }
    }

  private lazy val fragmentsRenderer: SelectionRenderer[Set[Selection.FragmentSpread], SelectionState] =
    set(fragmentDefinitionRenderer, ' ')

  private lazy val fragmentDefinitionRenderer: SelectionRenderer[Selection.FragmentSpread, SelectionState] =
    new SelectionRenderer[Selection.FragmentSpread, SelectionState] {
      override def unsafeRender(
        a: Selection.FragmentSpread,
        state0: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        writer.append("fragment")
        writer.append(' ')
        writer.append(a.name.getOrElse("f" + math.abs(a.code)))
        writer.append(" on ")
        writer.append(a.on)
        val state1       = selectionsRenderer.unsafeRender(a.selectionSet, state0, writer, options)
        val newFragments = state1.fragments -- state0.fragments
        if (newFragments.nonEmpty) {
          writer.append(' ')
          fragmentsRenderer.unsafeRender(newFragments, state1, writer, options)
        } else
          state1
      }
    }

  private lazy val fragmentSpreadInlineRenderer: SelectionRenderer[Selection.FragmentSpread, SelectionState] =
    new SelectionRenderer[Selection.FragmentSpread, SelectionState] {
      override def unsafeRender(
        a: Selection.FragmentSpread,
        state: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        writer.append("...")
        if (a.name.isDefined) writer.append(a.name.get)
        else writer.append('f').append(math.abs(a.code))

        state
      }
    }

  private lazy val argumentsRenderer: SelectionRenderer[List[Argument[_]], SelectionState] =
    list(argumentRenderer, ' ')

  private lazy val argumentRenderer: SelectionRenderer[Argument[_], SelectionState] =
    new SelectionRenderer[Argument[_], SelectionState] {
      override def unsafeRender(
        a: Argument[_],
        state: State,
        writer: StringBuilder,
        options: Options
      ): State =
        a.encodeRaw match {
          case `__NullValue` => state
          case v             =>
            val value = if (options.dropNullInputValues) v.dropNullValues else v
            if (options.useVariables) {
              val variableName = Argument.generateVariableName(a.name, value, state.variables)
              writer.append(a.name)
              writer.append(':')
              writer.append('$')
              writer.append(variableName)
              state.copy(variables = state.variables.updated(variableName, (value, a.typeInfo)))
            } else {
              writer.append(a.name)
              writer.append(':')
              writer.append(value.toString)
              state
            }
        }
    }

  private lazy val directivesRenderer =
    list(directiveRenderer, ' ')

  private lazy val directiveRenderer: SelectionRenderer[Directive, SelectionState] =
    new SelectionRenderer[Directive, SelectionState] {
      override def unsafeRender(
        a: Directive,
        state0: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        writer.append('@')
        writer.append(a.name)
        if (a.arguments.nonEmpty) {
          writer.append('(')
          val state = argumentsRenderer.unsafeRender(a.arguments, state0, writer, options)
          writer.append(')')
          state
        } else state0
      }
    }
}
