package caliban.execution

package object deferred extends DeferredExecutionSyntax {
    private def deferredFields(shouldDefer: FieldInfo => Boolean): FieldWrapper[Any] = 
          // TODO Change this to false, the spec says that we can
    //  ignore defer requests for some fields if it is more efficient to do so
    //  this will force pure fields to resolve immediately.
    new FieldWrapper[Any](true) {
      def wrap[R1](
        query: ZQuery[R1, ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R1, ExecutionError, ResponseValue] = {
        // If this field is deferred we will push it off into a DeferredValue that wraps an existing query
        hasDeferred(info).filter(_ => shouldDefer(info)).fold(query) {
          d => ZQuery.environment[R1].map(env => ResponseValue.DeferValue(
            query.provide(Described(env, "deferred field")),
            info.path,
            d.arguments.collectFirst {
              case ("label", Value.StringValue(s)) => s
            }.getOrElse("")
          ))
        }
      }
    }

  private def hasDeferred(field: FieldInfo): Option[Directive] = 
    field.details.fragment.flatMap(_.directives.collectFirst {
      case d if d.name == "defer" => 
        d
    })
  

  private lazy val deferredOverall: Wrapper.ExecutionWrapper[Any] =
    new Wrapper.ExecutionWrapper[Any] {
      override def wrap[R1](f: ExecutionRequest => ZIO[R1,Nothing,GraphQLResponse[CalibanError]]): ExecutionRequest => ZIO[R1,Nothing,GraphQLResponse[CalibanError]] = 
        (request: ExecutionRequest) => ZIO.environment[R1].flatMap(env => f(request).map { response =>
          // Extract the deferred values from the response and push them into the extension field instead
          val (eagerResponse, deferred) = extractDeferredValues(response.data, Nil)

          val stream: ZStream[R1, CalibanError.ExecutionError, ObjectValue] = ZStream.fromIterable(deferred.groupBy {
            case (name, value) =>  value.label -> value.path
          }).mapMPar(16) {
            case ((label, path), values) =>
              ZQuery.foreachBatched(values) {
                case (name, DeferValue(v, _, _)) =>
                  v.map(name -> _) 
              }.fold(
              error => ObjectValue(
                List(
                  "data" -> Value.NullValue,
                  "errors" -> ResponseValue.ListValue(Nil),
                  "label" -> StringValue(label),
                  "path" -> ListValue(path.map {
                    case Left(s) => StringValue(s)
                    case Right(i) => IntValue(i)
                  })
                )
              ), 
              data => ObjectValue(
                List(
                  "data" -> ObjectValue(data),
                  "label" -> StringValue(label),
                  "path" -> ListValue(path.map {
                    case Left(s) => StringValue(s)
                    case Right(i) => IntValue(i)
                  })
                )
              )).run
          }

          response.copy(
            data = eagerResponse,
            extensions = Some(response.extensions.foldLeft(ObjectValue(List("__defer" -> StreamValue(stream.provide(env)))))(
              (s, acc) => ObjectValue(s.fields ++ acc.fields))
            )
          )
        })
    }

  type Path = List[Either[String, Int]]

  lazy val deferredExecution: Wrapper[Any]  = deferredExecution(_ => true)

  def deferredExecution(shouldDefer: FieldInfo => Boolean): GraphQLAspect[Any] =
    deferredFields(shouldDefer) |+| deferredOverall

  private def extractDeferredValues(value: ResponseValue, path: List[Either[String, Int]]): (ResponseValue, List[(String, DeferValue)]) = 
    value match {
      case ResponseValue.ObjectValue(values) =>
        val out = List.newBuilder[(String, ResponseValue)]
        val deferred = List.newBuilder[(String, DeferValue)]
        values.foreach {
          case (key, d: DeferValue) => deferred.+=(key -> d)
          case (key, value) => 
            val (inner, d) = extractDeferredValues(value, Left(key) :: path)
            out.+=(key -> inner)
            deferred ++= d
        }
        ResponseValue.ObjectValue(out.result()) -> deferred.result()
      case ResponseValue.ListValue(values) =>
        val out = List.newBuilder[ResponseValue]
        val deferred = List.newBuilder[(String, DeferValue)]

        values.zipWithIndex.foreach {
          case (value, index) => 
            val (inner, d) = extractDeferredValues(value, Right(index) :: path)
            out += inner
            deferred ++= d
        }
        ResponseValue.ListValue(out.result()) -> deferred.result()
      case v => (v, Nil)
    }
}
