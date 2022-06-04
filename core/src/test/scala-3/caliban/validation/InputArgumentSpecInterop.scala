package caliban.validation

object InputArgumentSpecInterop {
  import InputArgumentSpec._

  case class Query(
    bool: BoolArg => String = _ => "result",
    boolNonNull: BoolArgNonNull => String = _ => "result",
    float: FloatArg => String = _ => "result",
    int: IntArg => String = _ => "result",
    list: ListArg => String = _ => "result",
    listInt: ListIntArg => Option[List[Option[Int]]] = _.input,
    // does not compile on Scala 3
    // listListInt: ListListIntArg => Option[List[Option[List[Option[Int]]]]] = _.input,
    string: StringArg => String = _ => "result",
    `enum`: EnumArg => String = _ => "result",
    input: InputArg => String = _ => "result",
    exampleObject: ExampleObjectArg => Option[ExampleObject] = _.input
  )
}
