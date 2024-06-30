package caliban.tools

import caliban.execution.Field
import caliban.parsing.adt.{ Document, Type }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.Validator
import zio.{ Exit, Task, ZIO, ZIOAppDefault }

import java.io.File
import scala.util.control.NoStackTrace

case class ExecutableQuery(
  query: String,
  variables: Map[String, Type],
  selection: List[ExecutableQuery.Selection]
)

object ExecutableQuery {
  case class Selection(name: String, `type`: Type, fields: List[Selection]) {
    lazy val fieldNames: List[String] = fields.map(_.name)
  }
}

object QueryLoader {

  def fromDirectory(schema: Document, pathName: String): Task[List[ExecutableQuery]] =
    loadDirectory(pathName).flatMap(extractQueryData(schema, _))

  def fromLoader(schema: Document, loader: SchemaLoader): Task[ExecutableQuery] =
    loader.load.flatMap(d => extractQueryData(schema, d :: Nil).map(_.head))

  private def loadDirectory(pathName: String): Task[List[Document]] =
    for {
      files <- ZIO.attemptBlocking(listGqlFiles(pathName))
      docs  <- ZIO.foreachPar(files)(SchemaLoader.fromFile(_).load)
    } yield docs

  private def extractQueryData(schema: Document, docs: List[Document]): Task[List[ExecutableQuery]] =
    for {
      rootType <- getRootType(schema)
      reqs     <- ZIO.foreach(docs)(d => Exit.fromEither(validateDocument(rootType, d)))
    } yield reqs

  private def getRootType(schema: Document) =
    ZIO
      .fromOption(parseRootType(schema))
      .orElse(ZIO.dieMessage("Failed to parse schema document"))

  private def parseRootType(doc: Document): Option[RootType] =
    RemoteSchema.parseRemoteSchema(doc).map { schema =>
      RootType(
        queryType = schema.queryType,
        mutationType = schema.mutationType,
        subscriptionType = schema.subscriptionType,
        additionalTypes = Nil,
        additionalDirectives = schema.directives,
        description = schema.description
      )
    }

  private def listGqlFiles(pathName: String): List[File] = {
    val source = new File(pathName)
    val files  = source.listFiles((_, fn) => fn.endsWith(".graphql") || fn.endsWith(".gql"))
    files.toList
  }

  private def validateDocument(schema: RootType, doc: Document): Either[Throwable, ExecutableQuery] =
    doc.operationDefinitions match {
      case op :: Nil =>
        Validator
          .prepare(
            document = doc,
            rootType = schema,
            operationName = None,
            variables = Map.empty,
            skipValidation = false,
            validations = List(
              Validator.validateFragmentSpreads,
              Validator.validateOperationNameUniqueness,
              Validator.validateLoneAnonymousOperation,
              Validator.validateDirectives,
              Validator.validateSubscriptionOperation,
              Validator.validateDocumentFields(validateFieldArgs = false)
            )
          )
          .map { req =>
            ExecutableQuery(
              DocumentRenderer.renderCompact(doc),
              op.variableDefinitions.map(d => d.name -> d.variableType).toMap,
              req.field.fields.map(extractSelections)
            )
          }
      case Nil       =>
        Left(GqlFileValidationError("GraphQL file did not contain any operations"))
      case _         =>
        Left(GqlFileValidationError("Currently only one operation per file is supported"))
    }

  private def extractSelections(field: Field): ExecutableQuery.Selection =
    ExecutableQuery.Selection(
      field.aliasedName,
      field.fieldType.toType(),
      field.fields.map(extractSelections)
    )

  private case class GqlFileValidationError(msg: String) extends Exception(msg) with NoStackTrace
}
