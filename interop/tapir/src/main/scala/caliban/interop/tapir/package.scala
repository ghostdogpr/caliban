package caliban.interop

import caliban.introspection.adt._
import caliban.schema.Step.{ FunctionStep, QueryStep }
import caliban.schema._
import caliban.wrappers.Wrapper
import caliban.{ GraphQL, InputValue }
import sttp.model.Method
import sttp.monad.MonadError
import sttp.tapir.internal._
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{ Endpoint, EndpointIO, EndpointInput, EndpointOutput }
import _root_.zio.query.{ URQuery, ZQuery }
import _root_.zio.{ IO, URIO, ZIO }

package object tapir {

  implicit class GraphQLInfallibleEndpoint[I, O](e: Endpoint[I, Nothing, O, Any]) {
    def toGraphQL[R](logic: I => URIO[R, O])(implicit
      inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(e.serverLogic[URQuery[R, *]](input => ZQuery.fromEffect(logic(input).map(Right(_)))))

    def toGraphQLQuery[R](logic: I => URQuery[R, O])(implicit
      inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(e.serverLogic[URQuery[R, *]](input => logic(input).map(Right(_))))
  }

  implicit class GraphQLEndpoint[I, E, O](e: Endpoint[I, E, O, Any]) {
    def toGraphQL[R](logic: I => ZIO[R, E, O])(implicit
      inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(e.serverLogic[URQuery[R, *]](input => ZQuery.fromEffect(logic(input).either)))

    def toGraphQLQuery[R](logic: I => ZQuery[R, E, O])(implicit
      inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(e.serverLogic[URQuery[R, *]](logic(_).either))
  }

  implicit class GraphQLInfallibleServerEndpoint[R, I, O](e: ServerEndpoint[I, Nothing, O, Any, URIO[R, *]]) {
    def toGraphQL(implicit
      inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(e.endpoint.serverLogic(input => ZQuery.fromEffect(e.logic(monadError)(input))))
  }

  implicit class GraphQLServerEndpoint[R, I, E, O](e: ServerEndpoint[I, E, O, Any, ZIO[R, E, *]]) {
    def toGraphQL(implicit
      inputSchema: caliban.schema.Schema[R, I],
      outputSchema: caliban.schema.Schema[R, O],
      argBuilder: ArgBuilder[I]
    ): GraphQL[R] =
      tapir.toGraphQL(
        e.endpoint.serverLogic(input => ZQuery.fromEffect(e.logic(monadError)(input).either.map(_.flatMap(identity))))
      )
  }

  def toGraphQL[R, I, E, O, S](serverEndpoint: ServerEndpoint[I, E, O, S, URQuery[R, *]])(implicit
    inputSchema: caliban.schema.Schema[R, I],
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
        case _                       =>
          argNames.values.headOption.flatten
            .fold(List.empty[__InputValue]) { case (name, desc) =>
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
              extractPath(serverEndpoint.endpoint.info.name, serverEndpoint.endpoint.input),
              serverEndpoint.endpoint.info.description,
              getArgs(inputSchema.toType_(isInput = true), inputSchema.optional),
              () =>
                if (serverEndpoint.endpoint.errorOutput == EndpointOutput.Void())
                  Types.makeNonNull(outputSchema.toType_())
                else outputSchema.toType_(),
              serverEndpoint.endpoint.info.deprecated
            )
          ),
          Nil
        ),
        Step.ObjectStep(
          name,
          Map(
            extractPath(serverEndpoint.endpoint.info.name, serverEndpoint.endpoint.input) ->
              FunctionStep { args =>
                val replacedArgs = args.map { case (k, v) => reverseArgNames.getOrElse(k, k) -> v }
                QueryStep(
                  ZQuery
                    .fromEffect(IO.fromEither(argBuilder.build(InputValue.ObjectValue(replacedArgs))))
                    .flatMap(input => serverEndpoint.logic(queryMonadError)(input))
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
        case _                                        =>
          RootSchemaBuilder(Some(makeOperation("Query")), None, None)
      }

    override protected val wrappers: List[Wrapper[R]]              = Nil
    override protected val additionalDirectives: List[__Directive] = Nil
  }

  private def extractPath[I](endpointName: Option[String], input: EndpointInput[I]): String =
    endpointName
      .map(replaceIllegalChars)
      .getOrElse(
        input
          .asVectorOfBasicInputs(includeAuth = false)
          .collect { case EndpointInput.FixedPath(s, _, _) =>
            s
          }
          .toList match {
          case Nil          => "root"
          case head :: Nil  => head
          case head :: tail => head ++ tail.map(_.capitalize).mkString
        }
      )

  private def replaceIllegalChars(s: String): String =
    s.replaceAll("\\W+", "_")

  private def extractArgNames[I](input: EndpointInput[I]): Map[String, Option[(String, Option[String])]] =
    input.traverseInputs {
      case EndpointInput.PathCapture(Some(name), _, info) => Vector(Some((name, info.description)))
      case EndpointInput.Query(name, _, info)             => Vector(Some((name, info.description)))
      case EndpointInput.Cookie(name, _, info)            => Vector(Some((name, info.description)))
      case EndpointIO.Header(name, _, info)               => Vector(Some((name, info.description)))
      case EndpointIO.Body(_, _, info)                    => Vector(Some(("body", info.description)))
      case _: EndpointInput.MappedPair[_, _, _, _]        => Vector(None)
      case _: EndpointIO.MappedPair[_, _, _, _]           => Vector(None)
    }.zipWithIndex.map { case (v, index) =>
      s"_${index + 1}" -> (v match {
        case None               => None
        case Some((name, desc)) => Some((name.replace("-", "_"), desc))
      })
    }.toMap

  private def monadError[R, E]: MonadError[ZIO[R, E, *]] = new MonadError[ZIO[R, E, *]] {
    def unit[T](t: T): ZIO[R, E, T]                                                                        = ZIO.succeed(t)
    def map[T, T2](fa: ZIO[R, E, T])(f: T => T2): ZIO[R, E, T2]                                            = fa.map(f)
    def flatMap[T, T2](fa: ZIO[R, E, T])(f: T => ZIO[R, E, T2]): ZIO[R, E, T2]                             = fa.flatMap(f)
    def error[T](t: Throwable): ZIO[R, E, T]                                                               = ZIO.die(t)
    def handleWrappedError[T](rt: ZIO[R, E, T])(h: PartialFunction[Throwable, ZIO[R, E, T]]): ZIO[R, E, T] =
      rt.catchSome { case e: Throwable =>
        h(e)
      }
    def ensure[T](f: ZIO[R, E, T], e: => ZIO[R, E, Unit]): ZIO[R, E, T]                                    = f.ensuring(e.ignore)
  }

  private def queryMonadError[R, E]: MonadError[ZQuery[R, E, *]] = new MonadError[ZQuery[R, E, *]] {
    def unit[T](t: T): ZQuery[R, E, T]                                                                              = ZQuery.succeed(t)
    def map[T, T2](fa: ZQuery[R, E, T])(f: T => T2): ZQuery[R, E, T2]                                               = fa.map(f)
    def flatMap[T, T2](fa: ZQuery[R, E, T])(f: T => ZQuery[R, E, T2]): ZQuery[R, E, T2]                             = fa.flatMap(f)
    def error[T](t: Throwable): ZQuery[R, E, T]                                                                     = ZQuery.die(t)
    def handleWrappedError[T](rt: ZQuery[R, E, T])(h: PartialFunction[Throwable, ZQuery[R, E, T]]): ZQuery[R, E, T] = rt
    def ensure[T](f: ZQuery[R, E, T], e: => ZQuery[R, E, Unit]): ZQuery[R, E, T]                                    =
      f.foldCauseM(cause => e.catchAll(_ => ZQuery.succeed(())) *> ZQuery.halt(cause), res => e.as(res))
  }
}
