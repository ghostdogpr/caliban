package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import caliban.parsing.adt.Definition._
import caliban.parsing.adt.Definition.ExecutableDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Selection._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import cats.parse.{Numbers, Parser => P, Parser1 => P1}
import cats.parse._
import cats.implicits._
import cats.parse.Parser.{not, string1}
import zio.IO

object Parser {

  private val sourceCharacter: P1[Char]                      = P.charIn("\u0009\u000A\u000D\u0020-\uFFFF")
  private val sourceCharacterWithoutLineTerminator: P1[Char] = P.charIn("\u0009\u0020-\uFFFF")

/*
  private val whitespace = {
    val UnicodeBOM   = '\uFEFF'
    val Tab          = '\u0009'
    val Space        = '\u0020'
    val LF           = '\u000A'
    val CR           = '\u000D'
    val Comma        = ','
    val CommentStart = '#'

    val normal = P.charIn(Space, Comma, Tab, UnicodeBOM)
  }
*/

/*
  private implicit val whitespace: P[_] => P[Unit] = new (P[_] => P[Unit]) {

    type State = Int

    // statuses
    private final val Normal                  = 0
    private final val InsideLineComment       = 1
    private final val DetermineLineBreakStart = 2

    private final val UnicodeBOM   = '\uFEFF'
    private final val Tab          = '\u0009'
    private final val Space        = '\u0020'
    private final val LF           = '\u000A'
    private final val CR           = '\u000D'
    private final val Comma        = ','
    private final val CommentStart = '#'

    override def apply(p: P[_]): P[Unit] =
      loop(p.index, state = Normal)(p, p.input)

    @tailrec def loop(index: Int, state: State)(implicit ctx: P[_], input: ParserInput): ParsingRun[Unit] =
      if (input.isReachable(index)) {
        val currentChar = input(index)
        (state: @switch) match {
          case Normal =>
            (currentChar: @switch) match {
              case Space | LF | Comma | Tab | UnicodeBOM => loop(index + 1, state = Normal)
              case CommentStart                          => loop(index + 1, state = InsideLineComment)
              case CR                                    => loop(index + 1, state = DetermineLineBreakStart)
              case _                                     => ctx.freshSuccessUnit(index)
            }
          case InsideLineComment =>
            loop(
              index + 1,
              state = (currentChar: @switch) match {
                case CR => DetermineLineBreakStart
                case LF => Normal
                case _  => InsideLineComment
              }
            )
          case DetermineLineBreakStart =>
            (currentChar: @switch) match {
              case LF => loop(index + 1, state = Normal)
              case _  => loop(index, state = Normal)
            }
        }
      } else ctx.freshSuccessUnit(index)
  }
*/

  private val name: P1[String] = string1(P.charIn("_A-Za-z").void ~ P.charIn("_0-9A-Za-z").void.rep)

  private val booleanValue: P1[BooleanValue] =
    P.string1("true").as(BooleanValue(true)) orElse1 P.string1("false").as(BooleanValue(false))

  private val intValue: P1[IntValue] = Numbers.signedIntString.map(IntValue(_))

  private val floatValue: P1[FloatValue] = Numbers.jsonNumber.map(FloatValue(_))

  private val hexdig: Parser1[Char] = Numbers.digit.orElse1(P.ignoreCaseCharIn('A' to 'F'))
  //todo: simplify
  private lazy val escapedUnicode: P1[String] =
    (hexdig, hexdig, hexdig, hexdig).tupled.map {
      case (h1, h2, h3, h4) => Integer.parseInt(String.valueOf(Array(h1, h2, h3, h4)), 16).toChar.toString
    }

  private val escapedCharacter: P1[String] = string1(P.charIn("\"\\/bfnrt")).map {
    case "b"   => "\b"
    case "n"   => "\n"
    case "f"   => "\f"
    case "r"   => "\r"
    case "t"   => "\t"
    case other => other
  }

  private val stringCharacter: P1[String] =
    string1(P.oneOf1(List(
      sourceCharacterWithoutLineTerminator.flatMap(c => if (c != '\"' && c != '\\') P.pure(c) else P.fail).void,
      (P.string1("\\u") ~ escapedUnicode).void,
      (P.string1("\\") ~ escapedCharacter).void
    )))

  private val blockStringCharacter: P1[String] = P.string1("\\\"\"\"").as("\"\"\"") orElse1 sourceCharacter.map(_.toString)

  private val stringValue: P1[StringValue] =
    (
      (P.string1("\"\"\"") *> blockStringCharacter.rep <* P.string1("\"\"\"")).map(s => blockStringValue(s.mkString)) orElse1
        (P.string1("\"") *> stringCharacter.rep <* P.string1("\"")).map(_.mkString)
    ).map(v => StringValue(v))

  private def blockStringValue(rawValue: String): String = {
    val l1 = rawValue.split("\r?\n").toList
    val commonIndent = l1 match {
      case Nil => None
      case _ :: tail =>
        tail.foldLeft(Option.empty[Int]) {
          case (commonIndent, line) =>
            val indent = "[ \t]*".r.findPrefixOf(line).fold(0)(_.length)
            if (indent < line.length && commonIndent.fold(true)(_ > indent)) Some(indent) else commonIndent
        }
    }
    // remove indentation
    val l2 = (commonIndent, l1) match {
      case (Some(value), head :: tail) => head :: tail.map(_.drop(value))
      case _                           => l1
    }
    // remove start lines that are only whitespaces
    val l3 = l2.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty)
    // remove end lines that are only whitespaces
    val l4 = l3.reverse.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty).reverse
    l4.mkString("\n")
  }

  private val nullValue: P1[InputValue] = P.string1("null").as(NullValue)
  private lazy val enumValue: P1[InputValue] = name.map(EnumValue)
  private lazy val listValue: P1[ListValue]  = (P.char('[') *> P.defer1(value).rep <* P.char(']')).map(ListValue)

  private lazy val objectField: P1[(String, InputValue)] = (name <* P.char(':')) ~ P.defer1(value)
  private lazy val objectValue: P1[ObjectValue] =
    (P.char('{') *> objectField.rep <* P.char('}')).map(values => ObjectValue(values.toMap))

  private lazy val variable: P1[VariableValue] = (P.char('$') *> name).map(VariableValue)

  private lazy val value: P1[InputValue] =
    P.oneOf1(floatValue :: intValue :: booleanValue :: stringValue :: nullValue :: enumValue :: listValue :: objectValue :: variable :: Nil)

  private lazy val argument: P1[(String, InputValue)]     = (name ~ P.char(':') ~ value).map(t => (t._1._1, t._2))
  private lazy val arguments: P1[Map[String, InputValue]] = (P.char('(') *> argument.rep <* P.char(')')).map(_.toMap)

  private lazy val directive: P1[Directive] = (P.index.with1 ~ P.char('@') ~ name ~ arguments.?).map {
    case (((index, _), name), arguments) => Directive(name, arguments.getOrElse(Map()), index)
  }
  private lazy val directives: P[List[Directive]] = directive.rep

  private lazy val defaultValue: P1[InputValue] = P.char('=') *> value
  private lazy val variableDefinition: P1[VariableDefinition] =
    ((variable <* P.char(':')) ~ _type ~ defaultValue.? ~ directives).map {
      case (((v, t), default), dirs) => VariableDefinition(v.name, t, default, dirs)
    }
  private lazy val variableDefinitions: P1[List[VariableDefinition]] = P.char('(') *> variableDefinition.rep <* P.char(')')

  private lazy val alias: P1[String] = string1(name ~ P.char(':'))

  private lazy val selection: P1[Selection]          = P.defer1(field) orElse1 P.defer1(fragmentSpread) orElse1 P.defer1(inlineFragment)
  private lazy val selectionSet: P1[List[Selection]] = (P.char('{') *> selection.rep1 <* P.char('}')).map(_.toList)

  private lazy val namedType: P1[NamedType] = name.flatMap(s => if (s == "null") P.fail else P.pure(s)).map(NamedType(_, nonNull = false))
  private lazy val listType: P1[ListType]   = (P.char('[') *> P.defer(_type) <* P.char(']')).map(t => ListType(t, nonNull = false))

  private lazy val argumentDefinition: P1[InputValueDefinition] =
    (stringValue.?.with1 ~ (name <* P.char(':')) ~ _type ~ defaultValue.? ~ directives.?).map {
      case ((((description, name), type_), defaultValue), directives) =>
        InputValueDefinition(description.map(_.value), name, type_, defaultValue, directives.getOrElse(Nil))
    }
  private lazy val argumentDefinitions: P[List[InputValueDefinition]] = P.char('(') *> argumentDefinition.rep <* P.char(')')

  private lazy val fieldDefinition: P1[FieldDefinition] =
    (stringValue.?.with1 ~ name ~ argumentDefinitions.? ~ (P.char(':') *> _type) ~ directives.?).map {
      case ((((description, name), args), type$), directives) =>
        FieldDefinition(description.map(_.value), name, args.getOrElse(Nil), type$, directives.getOrElse(Nil))
    }

  private lazy val nonNullType: P1[Type] = ((namedType orElse1 listType) <* P.char('!')).map {
    case t: NamedType => t.copy(nonNull = true)
    case t: ListType  => t.copy(nonNull = true)
  }
  private lazy val _type: P1[Type] = nonNullType orElse1 namedType orElse1 listType

  private lazy val field: P1[Field] = (P.index.with1 ~ (alias.?.with1 ~ name ~ arguments.? ~ directives.? ~ selectionSet.?)).map {
    case (index, ((((alias, name), args), dirs), sels)) =>
      Field(
        alias,
        name,
        args.getOrElse(Map()),
        dirs.map(_.toList).getOrElse(Nil),
        sels.getOrElse(Nil),
        index
      )
  }

  private lazy val fragmentName: P1[String] = name.flatMap(n => if (n != "on") P.pure(n) else P.fail)

  private lazy val fragmentSpread: P1[FragmentSpread] =
    (P.string1("...") *> (fragmentName ~ directives)).map {
      case (name, dirs) => FragmentSpread(name, dirs)
    }

  private lazy val typeCondition: P[NamedType] = P.string1("on") *> namedType

  private lazy val inlineFragment: P1[InlineFragment] =
    (P.string1("...") *> typeCondition.? ~ directives ~ selectionSet).map {
      case ((typeCondition, dirs), sel) => InlineFragment(typeCondition, dirs, sel)
    }

  private lazy val operationType: P1[OperationType] =
    P.oneOf1(List[P1[OperationType]](
      P.string1("query").as(OperationType.Query),
      P.string1("mutation").as(OperationType.Mutation),
      P.string1("subscription").as(OperationType.Subscription)
    ))

  private lazy val operationDefinition: P1[OperationDefinition] =
    (operationType ~ name.? ~ variableDefinitions.? ~ directives ~ selectionSet).map {
      case ((((operationType, name), variableDefinitions), directives), selection) =>
        OperationDefinition(operationType, name, variableDefinitions.getOrElse(Nil), directives, selection)
    } orElse1 selectionSet.map(selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection))

  private lazy val fragmentDefinition: P1[FragmentDefinition] =
    (P.string1("fragment") *> fragmentName ~ typeCondition ~ directives ~ selectionSet).map {
      case (((name, typeCondition), dirs), sel) => FragmentDefinition(name, typeCondition, dirs, sel)
    }

  private lazy val implements: P1[List[NamedType]] =
    (P.string1("implements") *> ((P.char('&').? *> namedType) ~ (P.char('&') *> namedType).rep)).map {
      case (head, tail) => head :: tail
    }

  private lazy val objectTypeDefinition: P1[ObjectTypeDefinition] =
    (stringValue.?.with1 ~ (P.string1("type") *> name) ~ implements.? ~ directives.? ~ (P.char('{') *> fieldDefinition.rep <* P.char('}'))).map {
      case ((((description, name), implements), directives), fields) =>
        ObjectTypeDefinition(
          description.map(_.value),
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields
        )
    }

  private lazy val interfaceTypeDefinition: P1[InterfaceTypeDefinition] =
    (stringValue.?.with1 ~ (P.string1("interface") *> name) ~ directives.? ~ (P.char('{') *> fieldDefinition.rep <* P.char('{'))).map {
      case (((description, name), directives), fields) =>
        InterfaceTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields)
    }

  private lazy val inputObjectTypeDefinition: P1[InputObjectTypeDefinition] =
    (stringValue.?.with1 ~ (P.string1("input") *> name) ~ directives.? ~ (P.char('{') *> argumentDefinition.rep <* P.char('}'))).map {
      case (((description, name), directives), fields) =>
        InputObjectTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields)
    }

  private lazy val enumValueDefinition: P1[EnumValueDefinition] =
    (stringValue.?.with1 ~ name ~ directives.?).map {
      case ((description, enumValue), directives) =>
        EnumValueDefinition(description.map(_.value), enumValue, directives.getOrElse(Nil))
    }

  private lazy val enumName: P1[String] = name.flatMap(s => if (s != "true" && s != "false" && s != "null") P.pure(s) else P.fail)

  private lazy val enumTypeDefinition: P1[EnumTypeDefinition] =
    (stringValue.?.with1 ~ (P.string1("enum") *> enumName) ~ directives.? ~ (P.char('{') *> enumValueDefinition.rep <* P.char('}'))).map {
      case (((description, name), directives), enumValuesDefinition) =>
        EnumTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), enumValuesDefinition)
    }

  private lazy val unionTypeDefinition: P1[UnionTypeDefinition] =
    (stringValue.?.with1 ~ (P.string1("union") *> name) ~ (directives.? <* P.char('=')) ~ (P.char('|').? *> namedType) ~ (P.char('|') *> namedType).rep).map {
      case ((((description, name), directives), m), ms) =>
        UnionTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), (m :: ms).map(_.name))
    }

  private lazy val scalarTypeDefinition: P1[ScalarTypeDefinition] =
    (stringValue.?.with1 ~ (P.string1("scalar") *> name) ~ directives.?).map {
      case ((description, name), directives) =>
        ScalarTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil))
    }

  private lazy val rootOperationTypeDefinition: P1[(OperationType, NamedType)] = (operationType ~ P.char(':') ~ namedType).map(t => (t._1._1, t._2))

  private lazy val schemaDefinition: P1[SchemaDefinition] =
    (P.string1("schema") *> directives.? ~ (P.char('{') *> rootOperationTypeDefinition.rep <* P.char('}'))).map {
      case (directives, ops) =>
        val opsMap = ops.toMap
        SchemaDefinition(
          directives.getOrElse(Nil),
          opsMap.get(OperationType.Query).map(_.name),
          opsMap.get(OperationType.Mutation).map(_.name),
          opsMap.get(OperationType.Subscription).map(_.name)
        )
    }

  private lazy val schemaExtensionWithOptionalDirectivesAndOperations: P[SchemaExtension] =
    (directives.? ~ (P.char('{') *> rootOperationTypeDefinition.rep <* P.char('}'))).map {
      case (directives, ops) =>
        val opsMap = ops.toMap
        SchemaExtension(
          directives.getOrElse(Nil),
          opsMap.get(OperationType.Query).map(_.name),
          opsMap.get(OperationType.Mutation).map(_.name),
          opsMap.get(OperationType.Subscription).map(_.name)
        )
    }

  private lazy val schemaExtensionWithDirectives: P[SchemaExtension] =
    directives.map(l => SchemaExtension(l, None, None, None))

  private lazy val schemaExtension: P1[SchemaExtension] =
    P.string1("extend schema") *> (schemaExtensionWithOptionalDirectivesAndOperations orElse schemaExtensionWithDirectives)

  private lazy val scalarTypeExtension: P1[ScalarTypeExtension] =
    (P.string1("extend scalar") *> (name ~ directives)).map {
      case (name, directives) =>
        ScalarTypeExtension(name, directives)
    }

  private lazy val objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields: P1[ObjectTypeExtension] =
    (name ~ implements.? ~ directives.? ~ (P.char('{') *> fieldDefinition.rep <* P.char('}'))).map {
      case (((name, implements), directives), fields) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields
        )
    }

  private lazy val objectTypeExtensionWithOptionalInterfacesAndDirectives: P[ObjectTypeExtension] =
    (name ~ implements.? ~ directives <* not(P.char('{') *> fieldDefinition.rep <* P.char('}'))).map {
      case ((name, implements), directives) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives,
          Nil
        )
    }

  private lazy val objectTypeExtensionWithInterfaces: P[ObjectTypeExtension] =
    (name ~ implements).map {
      case (name, implements) =>
        ObjectTypeExtension(
          name,
          implements,
          Nil,
          Nil
        )
    }

  private lazy val objectTypeExtension: P1[ObjectTypeExtension] =
    P.string1("extend type") *> (
      objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields orElse
        objectTypeExtensionWithOptionalInterfacesAndDirectives orElse
        objectTypeExtensionWithInterfaces
    )

  private lazy val interfaceTypeExtensionWithOptionalDirectivesAndFields: P1[InterfaceTypeExtension] =
    (name ~ directives.? ~ (P.char('{') *> fieldDefinition.rep <* P.char('}'))).map {
      case ((name, directives), fields) =>
        InterfaceTypeExtension(name, directives.getOrElse(Nil), fields)
    }

  private lazy val interfaceTypeExtensionWithDirectives: P1[InterfaceTypeExtension] =
    (name ~ directives).map {
      case (name, directives) =>
        InterfaceTypeExtension(name, directives, Nil)
    }

  private lazy val interfaceTypeExtension: P1[InterfaceTypeExtension] =
    P.string1("extend interface") *> (
      interfaceTypeExtensionWithOptionalDirectivesAndFields orElse1
        interfaceTypeExtensionWithDirectives
    )

  private lazy val unionTypeExtensionWithOptionalDirectivesAndUnionMembers: P1[UnionTypeExtension] =
    (name ~ (directives.? <* P.char('=')) ~ (P.char('|').? *> namedType) ~ (P.char('|') *> namedType).rep).map {
      case (((name, directives), m), ms) =>
        UnionTypeExtension(name, directives.getOrElse(Nil), (m :: ms).map(_.name))
    }

  private val unionTypeExtensionWithDirectives: P1[UnionTypeExtension] =
    (name ~ directives).map {
      case (name, directives) =>
        UnionTypeExtension(name, directives, Nil)
    }

  private val unionTypeExtension: P1[UnionTypeExtension] =
    P.string1("extend union") *> (
      unionTypeExtensionWithOptionalDirectivesAndUnionMembers orElse1
        unionTypeExtensionWithDirectives
    )

  private val enumTypeExtensionWithOptionalDirectivesAndValues: P[EnumTypeExtension] =
    (enumName ~ directives.? ~ (P.char('{') *> enumValueDefinition.rep <* P.char('}'))).map {
      case ((name, directives), enumValuesDefinition) =>
        EnumTypeExtension(name, directives.getOrElse(Nil), enumValuesDefinition.toList)
    }

  private val enumTypeExtensionWithDirectives: P[EnumTypeExtension] =
    (enumName ~ directives).map {
      case (name, directives) =>
        EnumTypeExtension(name, directives, Nil)
    }

  private val enumTypeExtension: P1[EnumTypeExtension] =
    P.string1("extend enum") *> (enumTypeExtensionWithOptionalDirectivesAndValues orElse enumTypeExtensionWithDirectives)

  private val inputObjectTypeExtensionWithOptionalDirectivesAndFields: P[InputObjectTypeExtension] =
    (name ~ directives.? ~ (P.char('{') *> argumentDefinition.rep <* P.char('}'))).map {
      case ((name, directives), fields) =>
        InputObjectTypeExtension(name, directives.getOrElse(Nil), fields)
    }

  private val inputObjectTypeExtensionWithDirectives: P[InputObjectTypeExtension] =
    (name ~ directives).map {
      case (name, directives) =>
        InputObjectTypeExtension(name, directives, Nil)
    }

  private val inputObjectTypeExtension: P1[InputObjectTypeExtension] =
    P.string1("extend input") *> (
      inputObjectTypeExtensionWithOptionalDirectivesAndFields orElse
        inputObjectTypeExtensionWithDirectives
    )

  private val directiveLocation: P1[DirectiveLocation] =
    P.oneOf1(List(
      P.string1("QUERY").as(ExecutableDirectiveLocation.QUERY),
      P.string1("MUTATION").as(ExecutableDirectiveLocation.MUTATION),
      P.string1("SUBSCRIPTION").as(ExecutableDirectiveLocation.SUBSCRIPTION),
      P.string1("FIELD").as(ExecutableDirectiveLocation.FIELD),
      P.string1("FRAGMENT_DEFINITION").as(ExecutableDirectiveLocation.FRAGMENT_DEFINITION),
      P.string1("FRAGMENT_SPREAD").as(ExecutableDirectiveLocation.FRAGMENT_SPREAD),
      P.string1("INLINE_FRAGMENT").as(ExecutableDirectiveLocation.INLINE_FRAGMENT),
      P.string1("SCHEMA").as(TypeSystemDirectiveLocation.SCHEMA),
      P.string1("SCALAR").as(TypeSystemDirectiveLocation.SCALAR),
      P.string1("OBJECT").as(TypeSystemDirectiveLocation.OBJECT),
      P.string1("FIELD_DEFINITION").as(TypeSystemDirectiveLocation.FIELD_DEFINITION),
      P.string1("ARGUMENT_DEFINITION").as(TypeSystemDirectiveLocation.ARGUMENT_DEFINITION),
      P.string1("INTERFACE").as(TypeSystemDirectiveLocation.INTERFACE),
      P.string1("UNION").as(TypeSystemDirectiveLocation.UNION),
      P.string1("ENUM").as(TypeSystemDirectiveLocation.ENUM),
      P.string1("ENUM_VALUE").as(TypeSystemDirectiveLocation.ENUM_VALUE),
      P.string1("INPUT_OBJECT").as(TypeSystemDirectiveLocation.INPUT_OBJECT),
      P.string1("INPUT_FIELD_DEFINITION").as(TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION)
    ))

  private val directiveDefinition: P1[DirectiveDefinition] =
    (
      stringValue.?.with1 ~ (P.string1("directive @") *> name) ~ argumentDefinitions.? ~ (P.string1("on") *> ((P.char('|').soft *> directiveLocation) ~ (P.char('|') *> directiveLocation).rep))
    ).map {
      case (((description, name), args), (firstLoc, otherLoc)) =>
        DirectiveDefinition(description.map(_.value), name, args.getOrElse(Nil), otherLoc.toSet + firstLoc)
    }

  private val typeDefinition: P1[TypeDefinition] = P.oneOf1(List(
    objectTypeDefinition,
    interfaceTypeDefinition,
    inputObjectTypeDefinition,
    enumTypeDefinition,
    unionTypeDefinition,
    scalarTypeDefinition
  ))

  private val typeSystemDefinition: P1[TypeSystemDefinition] =
    P.oneOf1(typeDefinition :: schemaDefinition :: directiveDefinition :: Nil)

  private val executableDefinition: P1[ExecutableDefinition] =
    operationDefinition orElse1 fragmentDefinition

  private val typeExtension: P1[TypeExtension] = P.oneOf1(List(
    objectTypeExtension,
    interfaceTypeExtension,
    inputObjectTypeExtension,
    enumTypeExtension,
    unionTypeExtension,
    scalarTypeExtension
  ))

  private val typeSystemExtension: P1[TypeSystemExtension] =
    schemaExtension orElse1 typeExtension

  private val definition: P1[Definition] = P.oneOf1(executableDefinition :: typeSystemDefinition :: typeSystemExtension :: Nil)

  private val document: P[List[Definition]] = P.start *> definition.rep <* P.end

  /**
   * Parses the given string into a [[caliban.parsing.adt.Document]] object or fails with a [[caliban.CalibanError.ParsingError]].
   */
  def parseQuery(query: String): IO[ParsingError, Document] = {
    val sm = SourceMapper(query)
    IO.fromEither(document.parse(query))
      .bimap(
        error  => ParsingError(error.toString, Some(sm.getLocation(error.failedAtOffset))), //fixme: should be replaced with error msg
        result => Document(result._2, sm)
      )
  }

  /**
   * Checks if the query is valid, if not returns an error string.
   */
  def check(query: String): Option[String] = document.parse(query) match {
    case Left(error) => Some(error.failedAtOffset.toString) //fixme: should be replaced with error msg
    case Right(_)    => None
  }
}
