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
import zquery.{ URQuery, ZQuery }

package object tapir {

  implicit class GraphQLInfallibleEndpoint[I, O](e: Endpoint[I, Nothing, O, Nothing]) {
    def toGraphQL[R](logic: I => URIO[R, O])(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(
        ServerEndpoint[I, Nothing, O, Nothing, URQuery[R, *]](
          e,
          (input: I) => ZQuery.fromEffect(logic(input).map(Right(_)))
        )
      )

    def toGraphQLQuery[R](logic: I => URQuery[R, O])(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(
        ServerEndpoint[I, Nothing, O, Nothing, URQuery[R, *]](e, (input: I) => logic(input).map(Right(_)))
      )
  }

  implicit class GraphQLEndpoint[I, E, O](e: Endpoint[I, E, O, Nothing]) {
    def toGraphQL[R](logic: I => ZIO[R, E, O])(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(
        ServerEndpoint[I, E, O, Nothing, URQuery[R, *]](e, (input: I) => ZQuery.fromEffect(logic(input).either))
      )

    def toGraphQLQuery[R](logic: I => ZQuery[R, E, O])(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(
        ServerEndpoint[I, E, O, Nothing, URQuery[R, *]](e, (input: I) => logic(input).either)
      )
  }

  implicit class GraphQLInfallibleServerEndpoint[R, I, O](e: ServerEndpoint[I, Nothing, O, Nothing, URIO[R, *]]) {
    def toGraphQL(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] = tapir.toGraphQL(
      ServerEndpoint[I, Nothing, O, Nothing, URQuery[R, *]](e.endpoint, (input: I) => ZQuery.fromEffect(e.logic(input)))
    )
  }

  implicit class GraphQLServerEndpoint[R, I, E, O](e: ServerEndpoint[I, E, O, Nothing, ZIO[R, E, *]]) {
    def toGraphQL(
      implicit inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(
        ServerEndpoint[I, E, O, Nothing, URQuery[R, *]](
          e.endpoint,
          (input: I) => ZQuery.fromEffect(e.logic(input).either.map(_.flatMap(identity)))
        )
      )
  }

  def toGraphQL[R, I, E, O, S](serverEndpoint: ServerEndpoint[I, E, O, S, URQuery[R, *]])(
    implicit inputSchema: caliban.schema.Schema[R, I],
    outputSchema: caliban.schema.Schema[R, O],
    argBuilder: ArgBuilder[I]
  ): GraphQL[R] = new GraphQL[R] {

    val argNames: Map[String, Option[(String, Option[String])]] = extractArgNames(serverEndpoint.endpoint.input)
    val reverseArgNames: Map[String, String]                    = argNames.collect { case (k, Some((v, _))) => v -> k }

    def getArgs(t: __Type, optional: Boolean): List[__InputValue] =
      t.kind match {
        case __TypeKind.INPUT_OBJECT =>
          val fields = t.inputFields.getOrElse(Nil)
          if (fields.forall(_.name.matches(s"_[0-9]+")) && fields.length == argNames.size) {
            fields.map(f =>
              argNames.get(f.name).flatten match {
                case Some((name, desc)) => f.copy(name = name, description = desc)
                case None               => f
              }
            )
          } else fields
        case _ =>
          argNames.values.headOption.flatten
            .fold(List.empty[__InputValue]) {
              case (name, desc) =>
                List(__InputValue(name, desc, () => if (optional) t else Types.makeNonNull(t), None))
            }
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
              FunctionStep { args =>
                val replacedArgs = args.map { case (k, v) => reverseArgNames.getOrElse(k, k) -> v }
                QueryStep(
                  ZQuery
                    .fromEffect(IO.fromEither(argBuilder.build(InputValue.ObjectValue(replacedArgs))))
                    .flatMap(input => serverEndpoint.logic(input))
                    .map {
                      case Left(error: Throwable) => QueryStep(ZQuery.fail(error))
                      case Left(otherError)       => QueryStep(ZQuery.fail(new Throwable(otherError.toString)))
                      case Right(output)          => outputSchema.resolve(output)
                    }
                )
              }
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

  private def extractArgNames[I](input: EndpointInput[I]): Map[String, Option[(String, Option[String])]] =
    input.traverseInputs {
      case EndpointInput.PathCapture(Some(name), _, info) => Vector(Some((name, info.description)))
      case EndpointInput.Query(name, _, info)             => Vector(Some((name, info.description)))
      case EndpointInput.Cookie(name, _, info)            => Vector(Some((name, info.description)))
      case EndpointIO.Header(name, _, info)               => Vector(Some((name, info.description)))
      case EndpointIO.Body(_, _, info)                    => Vector(Some(("body", info.description)))
      case _: EndpointInput.MappedPair[_, _, _, _]        => Vector(None)
      case _: EndpointIO.MappedPair[_, _, _, _]           => Vector(None)
    }.zipWithIndex.map {
      case (v, index) =>
        s"_${index + 1}" -> (v match {
          case None               => None
          case Some((name, desc)) => Some((name.replace("-", "_"), desc))
        })
    }.toMap
}
