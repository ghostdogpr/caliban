package codegen

import java.net.{ URL, URLClassLoader }
import java.nio.file.{ FileSystems, Paths }

import caliban.parsing.adt.{ Document, Selection, Type }
import caliban.parsing.adt.Document._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import com.typesafe.config.ConfigFactory
import org.scalafmt.dynamic.utils.ReentrantCache
import org.scalafmt.dynamic.{ ScalafmtDynamic, ScalafmtReflect }
import org.scalafmt.interfaces.Scalafmt
import zio.{ IO, Task }

object Generator {
  def generate(doc: Document)(implicit writerContext: GQLWriterContext): String =
    writerContext.docWriter.write(doc)(Nil)

  def format(str: String, fmtPath: String): Task[String] = Task {
    val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
    val config   = Paths.get(fmtPath)
    scalafmt.format(config, Paths.get("Nil.scala"), str)
  }

  def formatStr(code: String, fmt: String): Task[String] = Task {
    ReentrantCache()
    val scalafmtReflect =
      ScalafmtReflect(new URLClassLoader(new Array[URL](0), this.getClass.getClassLoader), "2.2.1", false)
    val config = scalafmtReflect.parseConfigFromString(fmt)

    scalafmtReflect.format(code, config)
  }

  trait GQLWriter[A, D] {
    def write(entity: A)(depends: D)(implicit context: GQLWriterContext): String
  }

  trait GQLWriterContext {
    implicit val fieldWriter: GQLWriter[FieldDefinition, TypeDefinition]
    implicit val typeWriter: GQLWriter[Type, Any]
    implicit val typeDefWriter: GQLWriter[TypeDefinition, Document]
    implicit val docWriter: GQLWriter[Document, Any]
    implicit val rootQueryWriter: GQLWriter[RootQueryDef, Document]
    implicit val queryWriter: GQLWriter[QueryDef, Document]
    implicit val rootMutationWriter: GQLWriter[RootMutationDef, Document]
    implicit val mutationWriter: GQLWriter[MutationDef, Document]
    implicit val rootSubscriptionWriter: GQLWriter[RootSubscriptionDef, Document]
    implicit val subscriptionWriter: GQLWriter[SubscriptionDef, Document]
    implicit val argsWriter: GQLWriter[Args, String]
  }

  case class RootQueryDef(op: TypeDefinition)
  case class QueryDef(op: FieldDefinition)

  case class RootMutationDef(op: TypeDefinition)
  case class MutationDef(op: FieldDefinition)

  case class RootSubscriptionDef(op: TypeDefinition)
  case class SubscriptionDef(op: FieldDefinition)

  case class Args(field: FieldDefinition)

  case class SubscriptionDefinition(op: OperationDefinition)

  object GQLWriter {
    def apply[A, D](implicit instance: GQLWriter[A, D]): GQLWriter[A, D] =
      instance
  }
}
