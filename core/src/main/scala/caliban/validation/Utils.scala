package caliban.validation

import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.adt.Type.NamedType
import zio.Chunk

import scala.collection.compat._

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

  def getType(t: Option[NamedType], parentType: __Type, context: Context): __Type =
    t.fold(Option(parentType))(t => context.rootType.types.get(t.name)).getOrElse(parentType)

  def getType(t: NamedType, context: Context): Option[__Type] =
    context.rootType.types.get(t.name)

  /**
   * For an iterable, produce a Chunk containing tuples of all possible unique combinations, optionally including the identity
   */
  def cross[A](a: Iterable[A], includeIdentity: Boolean): Chunk[(A, A)] = {
    val ca       = Chunk.fromIterable(a)
    val size     = ca.size
    val cb       = Chunk.newBuilder[(A, A)]
    var i1, i2   = 0
    val modifier = if (includeIdentity) 0 else 1
    while (i1 < size - modifier) {
      i2 = i1 + modifier
      val l = ca(i1)
      while (i2 < size) {
        cb += ((l, ca(i2)))
        i2 += 1
      }
      i1 += 1
    }
    cb.result()
  }

}
