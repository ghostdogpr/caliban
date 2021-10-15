package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.schema.{ RootSchema, RootSchemaBuilder, RootType, Types }
import caliban.{ InputValue, Rendering, Value }
import zio.{ Chunk, IO }

object Utils {
  def isObjectType(t: __Type): Boolean =
    t.kind match {
      case OBJECT => true
      case _      => false
    }

  def isConcrete(t: __Type): Boolean = !isAbstract(t)

  def isLeafType(t: __Type): Boolean = isEnum(t) || isScalar(t)

  def isEnum(t: __Type): Boolean = t.kind match {
    case ENUM => true
    case _    => false
  }

  def isComposite(t: __Type): Boolean =
    isObjectType(t) || isInterface(t) || isUnion(t)

  def isInterface(t: __Type): Boolean = t.kind match {
    case INTERFACE => true
    case _         => false
  }

  def isUnion(t: __Type): Boolean = t.kind match {
    case UNION => true
    case _     => false
  }

  def isScalar(t: __Type): Boolean = t.kind match {
    case SCALAR => true
    case _      => false
  }

  def isAbstract(t: __Type): Boolean =
    t.kind match {
      case UNION     => true
      case INTERFACE => true
      case _         => false
    }

  def isNonNull(t: __Type): Boolean = t.kind == __TypeKind.NON_NULL

  def isListType(t: __Type): Boolean = t.kind == __TypeKind.LIST

  def getFields(t: __Type)                                                = t.fields(__DeprecatedArgs(Some(true)))
  def getType(t: Option[NamedType], parentType: __Type, context: Context) =
    t.fold(Option(parentType))(t => context.rootType.types.get(t.name)).getOrElse(parentType)

  def getType(t: NamedType, context: Context) =
    context.rootType.types.get(t.name)

  def cross[A](a: Iterable[A]): Chunk[(A, A)]                 =
    Chunk.fromIterable(for (xs <- a; ys <- a) yield (xs, ys))

  def cross[A](a: Iterable[A], b: Iterable[A]): Chunk[(A, A)] =
    Chunk.fromIterable(for (xs <- a; ys <- b) yield (xs, ys))

  object syntax {
    implicit class OptionSyntax[+A](val self: Option[A]) extends AnyVal {
      def zip[B](that: Option[B]): Option[(A, B)] =
        self.flatMap(a => that.map(b => (a, b)))
    }

    implicit class Tuple2Syntax[+A, +B](val self: Tuple2[Option[A], Option[B]]) extends AnyVal {
      def mapN[C](f: (A, B) => C): Option[C] =
        self._1.flatMap(a => self._2.map(b => f(a, b)))
    }

  }
}
