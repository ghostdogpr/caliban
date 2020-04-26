package caliban.interop

import caliban.introspection.adt._
import caliban.schema.Step.{ FunctionStep, QueryStep }
import caliban.schema._
import caliban.wrappers.Wrapper
import caliban.{ GraphQL, InputValue }
import sttp.model.Method
import sttp.tapir.internal._
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{ Endpoint, EndpointIO, EndpointInput, EndpointOutput }
import zio.{ IO, URIO, ZIO }
import zquery.ZQuery

/* TODO
- naming of arguments in tuples
- streaming
- implicit class for ServerEndpoint
- support for ZQuery
- documentation
 */

package object tapir {

  implicit class GraphQLInfallibleEndpoint[I, O, S](e: Endpoint[I, Nothing, O, S]) {
    def toGraphQL[R](logic: I => URIO[R, O])(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(ServerEndpoint[I, Nothing, O, S, URIO[R, *]](e, (input: I) => logic(input).map(Right(_))))
  }

  implicit class GraphQLEndpoint[I, E, O, S](e: Endpoint[I, E, O, S]) {
    def toGraphQL[R](logic: I => ZIO[R, E, O])(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(ServerEndpoint[I, E, O, S, URIO[R, *]](e, (input: I) => logic(input).either))
  }

  def toGraphQL[R, I, E, O, S](serverEndpoint: ServerEndpoint[I, E, O, S, URIO[R, *]])(
    implicit inputSchema: caliban.schema.Schema[R, I],
    outputSchema: caliban.schema.Schema[R, O],
    argBuilder: ArgBuilder[I]
  ): GraphQL[R] = new GraphQL[R] {

    val argNames: Map[Int, Option[String]] = extractArgNames(serverEndpoint.endpoint.input)

    def getArgs(t: __Type, optional: Boolean): List[__InputValue] =
      t.kind match {
        case __TypeKind.INPUT_OBJECT => t.inputFields.getOrElse(Nil)
        case _ =>
          argNames
            .get(0)
            .flatten
            .fold(List.empty[__InputValue])(arg =>
              List(__InputValue(arg, None, () => if (optional) t else Types.makeNonNull(t), None))
            )
      }

    def makeOperation(name: String): Operation[R] =
      Operation[R](
        Types.makeObject(
          Some(name),
          None,
          List(
            __Field(
              extractPath(serverEndpoint.endpoint.input),
              serverEndpoint.endpoint.info.description,
              getArgs(inputSchema.toType(true), inputSchema.optional),
              () =>
                if (serverEndpoint.endpoint.errorOutput == EndpointOutput.Void())
                  Types.makeNonNull(outputSchema.toType())
                else outputSchema.toType(),
              serverEndpoint.endpoint.info.deprecated
            )
          ),
          Nil
        ),
        Step.ObjectStep(
          name,
          Map(
            extractPath(serverEndpoint.endpoint.input) ->
              FunctionStep(args =>
                QueryStep(
                  ZQuery.fromEffect(
                    IO.fromEither(argBuilder.build(InputValue.ObjectValue(args)))
                      .flatMap(input => serverEndpoint.logic(input))
                      .map {
                        case Left(error: Throwable) => QueryStep(ZQuery.fail(error))
                        case Left(otherError)       => QueryStep(ZQuery.fail(new Throwable(otherError.toString)))
                        case Right(output)          => outputSchema.resolve(output)
                      }
                  )
                )
              )
          )
        )
      )

    override protected val schemaBuilder: RootSchemaBuilder[R] =
      serverEndpoint.endpoint.httpMethod.getOrElse(Method.GET) match {
        case Method.PUT | Method.POST | Method.DELETE =>
          RootSchemaBuilder(None, Some(makeOperation("Mutation")), None)
        case _ =>
          RootSchemaBuilder(Some(makeOperation("Query")), None, None)
      }

    override protected val wrappers: List[Wrapper[R]]              = Nil
    override protected val additionalDirectives: List[__Directive] = Nil
  }

  private def extractPath[I](input: EndpointInput[I]): String =
    input
      .asVectorOfBasicInputs(includeAuth = false)
      .collect {
        case EndpointInput.FixedPath(s, _, _) => s
      }
      .toList match {
      case Nil          => "root"
      case head :: Nil  => head
      case head :: tail => head ++ tail.map(_.capitalize).mkString
    }

  private def extractArgNames[I](input: EndpointInput[I]): Map[Int, Option[String]] =
    input.traverseInputs {
      case EndpointInput.PathCapture(Some(name), _, _) => Vector(Some(name))
      case EndpointInput.Query(name, _, _)             => Vector(Some(name))
      case EndpointInput.Cookie(name, _, _)            => Vector(Some(name))
      case _: EndpointInput.MappedTuple[_, _]          => Vector(None)
      case _: EndpointIO.MappedTuple[_, _]             => Vector(None)
    }.zipWithIndex.map { case (k, v) => v -> k }.toMap
}
