package caliban.client

import caliban.client.Renderer.{ list, set }
import caliban.client.Selection.Directive

import caliban.client.__Value.__NullValue

import scala.annotation.tailrec

private[client] case class RequestOptions(
  useVariables: Boolean,
  dropNullInputValues: Boolean,
  operationName: String,
  queryName: Option[String],
  directives: List[Directive]
)

private[client] object RequestRenderer
    extends Renderer[List[Selection], Map[String, (__Value, String)], RequestOptions] {
  import caliban.client.SelectionRenderer.{ directivesRenderer, selections, variablesRenderer }
  override def unsafeRender(a: List[Selection], state: State, writer: StringBuilder, options: RequestOptions): State = {
    val state0       = SelectionRenderer.RenderState(state, Set.empty)
    val inner        = new StringBuilder()
    val innerOptions = SelectionRenderer.Options(
      useVariables = options.useVariables,
      dropNullInputValues = options.dropNullInputValues
    )

    writer.append(options.operationName)
    options.queryName.foreach { queryName =>
      writer.append(' ')
      writer.append(queryName)
    }
    val state1 = selections.unsafeRender(a, state0, inner, innerOptions)
    val state2 = SelectionRenderer.fragments.unsafeRender(state1.fragments, state1, inner, innerOptions)
    val state3 = if (options.directives.nonEmpty) {
      writer.append(' ')
      directivesRenderer.unsafeRender(options.directives, state2, writer, innerOptions)
    } else state2

    variablesRenderer.unsafeRender(state3.variables, (), writer, innerOptions)
    writer.append(inner)
    state3.variables
  }
}

private[client] object SelectionRenderer {
  type SelectionRenderer[-A] = Renderer[A, RenderState, Options]

  case class Options(
    useVariables: Boolean,
    dropNullInputValues: Boolean
  )

  case class RenderState(
    variables: Map[String, (__Value, String)],
    fragments: Set[Selection.FragmentSpread]
  )

  lazy val selections: SelectionRenderer[List[Selection]] =
    new SelectionRenderer[List[Selection]] {
      override def unsafeRender(
        a: List[Selection],
        state: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        @tailrec
        def loop(
          sel: List[Selection],
          state0: State,
          names: Set[String],
          first: Boolean = false
        ): State =
          sel match {
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
              var state1     = selections.unsafeRender(f.selectionSet, state0, bodyWriter, options)
              if (f.arguments.nonEmpty) {
                state1 = argumentsRenderer.unsafeRender(f.arguments, state1, writer, options)
              }
              if (f.directives.nonEmpty) {
                writer.append(' ')
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

  lazy val fragments: SelectionRenderer[Set[Selection.FragmentSpread]] =
    set(fragmentDefinitionRenderer, ' ')

  lazy val variablesRenderer: Renderer[Map[String, (__Value, String)], Unit, Options] =
    new Renderer[Map[String, (__Value, String)], Unit, Options] {
      private val listRenderer: Renderer[List[(String, String)], Unit, Options] =
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

  private lazy val variableRenderer: Renderer[(String, String), Unit, Options] =
    new Renderer[(String, String), Unit, Options] {
      override def unsafeRender(a: (String, String), state: Unit, writer: StringBuilder, options: Options): Unit = {
        writer.append('$')
        writer.append(a._1)
        writer.append(": ")
        writer.append(a._2)
        ()
      }
    }

  private lazy val inlineFragmentRenderer: SelectionRenderer[Selection.InlineFragment] =
    new SelectionRenderer[Selection.InlineFragment] {
      override def unsafeRender(
        a: Selection.InlineFragment,
        state: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        writer.append("... on ")
        writer.append(a.onType)
        selections.unsafeRender(a.selectionSet, state, writer, options)
      }
    }

  private lazy val fragmentDefinitionRenderer: SelectionRenderer[Selection.FragmentSpread] =
    new SelectionRenderer[Selection.FragmentSpread] {
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
        val state1       = selections.unsafeRender(a.selectionSet, state0, writer, options)
        val newFragments = state1.fragments -- state0.fragments
        if (newFragments.nonEmpty) {
          writer.append(' ')
          fragments.unsafeRender(newFragments, state1, writer, options)
        } else
          state1
      }
    }

  private lazy val fragmentSpreadInlineRenderer: SelectionRenderer[Selection.FragmentSpread] =
    new SelectionRenderer[Selection.FragmentSpread] {
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

  private lazy val argumentsRenderer: SelectionRenderer[List[Argument[_]]] =
    new SelectionRenderer[List[Argument[_]]] {
      override def unsafeRender(
        a: List[Argument[_]],
        state0: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        @tailrec
        def loop(args: List[Argument[_]], state: State, first: Boolean): State =
          args match {
            case Nil       =>
              if (first) writer.append(')')
              state
            case a :: rest =>
              a.encodeRaw match {
                case `__NullValue` => loop(rest, state, first)
                case v             =>
                  if (first) writer.append(',')
                  else writer.append('(')
                  val value = if (options.dropNullInputValues) v.dropNullValues else v
                  if (options.useVariables) {
                    val variableName = Argument.generateVariableName(a.name, value, state.variables)
                    writer.append(a.name)
                    writer.append(':')
                    writer.append('$')
                    writer.append(variableName)
                    val state1       = state.copy(variables = state.variables.updated(variableName, (value, a.typeInfo)))
                    loop(rest, state1, first = true)
                  } else {
                    writer.append(a.name)
                    writer.append(':')
                    writer.append(value.toString)
                    loop(rest, state, first = true)
                  }
              }

          }

        loop(a, state0, first = false)
      }
    }

  lazy val directivesRenderer =
    list(directiveRenderer, ' ')

  private lazy val directiveRenderer: SelectionRenderer[Directive] =
    new SelectionRenderer[Directive] {
      override def unsafeRender(
        a: Directive,
        state0: State,
        writer: StringBuilder,
        options: Options
      ): State = {
        writer.append('@')
        writer.append(a.name)
        if (a.arguments.nonEmpty) {
          val state = argumentsRenderer.unsafeRender(a.arguments, state0, writer, options)
          state
        } else state0
      }
    }
}
