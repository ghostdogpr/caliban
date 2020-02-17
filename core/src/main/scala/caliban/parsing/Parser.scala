package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import caliban.parsing.adt.Definition._
import caliban.parsing.adt.Definition.ExecutableDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Selection._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import fastparse._
import zio.{ IO, Task }

object Parser {

  private def sourceCharacter[_: P]: P[Unit]                      = P(CharIn("\u0009\u000A\u000D\u0020-\uFFFF"))
  private def sourceCharacterWithoutLineTerminator[_: P]: P[Unit] = P(CharIn("\u0009\u0020-\uFFFF"))

  private def unicodeBOM[_: P]: P[Unit]     = P("\uFEFF")
  private def whiteSpace[_: P]: P[Unit]     = P(CharIn("\u0009\u0020"))
  private def lineTerminator[_: P]: P[Unit] = P("\u000A" | "\u000D" ~~ !"\u000A" | "\u000D\u000A")
  private def comma[_: P]: P[Unit]          = P(",")
  private def commentChar[_: P]: P[Unit]    = P(sourceCharacterWithoutLineTerminator)
  private def comment[_: P]: P[Unit]        = P("#" ~~ commentChar.repX)
  private def ignored[_: P]: P[Unit]        = P(unicodeBOM | whiteSpace | lineTerminator | comment | comma).repX

  implicit val whitespace: P[_] => P[Unit] = { implicit ctx: ParsingRun[_] =>
    ignored
  }

  private def name[_: P]: P[String] = P(CharIn("_A-Za-z") ~~ CharIn("_0-9A-Za-z").repX).!

  private def booleanValue[_: P]: P[BooleanValue] =
    P("true").map(_ => BooleanValue(true)) | P("false").map(_ => BooleanValue(false))

  private def negativeSign[_: P]: P[Unit] = P("-")
  private def nonZeroDigit[_: P]: P[Unit] = P(CharIn("1-9"))
  private def digit[_: P]: P[Unit]        = P("0" | nonZeroDigit)
  private def integerPart[_: P]: P[Unit]  = P((negativeSign.? ~~ "0") | (negativeSign.? ~~ nonZeroDigit ~~ digit.repX))
  private def intValue[_: P]: P[IntValue] = integerPart.!.map(IntValue(_))

  private def sign[_: P]: P[Unit]              = P("-" | "+")
  private def exponentIndicator[_: P]: P[Unit] = P(CharIn("eE"))
  private def exponentPart[_: P]: P[Unit]      = P(exponentIndicator ~~ sign.? ~~ digit.repX(1))
  private def fractionalPart[_: P]: P[Unit]    = P("." ~~ digit.repX(1))

  private def floatValue[_: P]: P[FloatValue] =
    P((integerPart ~~ fractionalPart) | (integerPart ~~ exponentPart) | (integerPart ~~ fractionalPart ~~ exponentPart)).!.map(
      FloatValue(_)
    )

  private def hexDigit[_: P]: P[Unit] = P(CharIn("0-9a-fA-F"))
  private def escapedUnicode[_: P]: P[String] =
    P(hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit).!.map(Integer.parseInt(_, 16).toChar.toString)

  private def escapedCharacter[_: P]: P[String] = P(CharIn("\"\\/bfnrt").!).map {
    case "b"   => "\b"
    case "n"   => "\n"
    case "f"   => "\f"
    case "r"   => "\r"
    case "t"   => "\t"
    case other => other
  }

  private def stringCharacter[_: P]: P[String] =
    P(
      sourceCharacterWithoutLineTerminator.!.filter(c => c != "\"" && c != "\\") | "\\u" ~~ escapedUnicode | "\\" ~~ escapedCharacter
    )

  private def blockStringCharacter[_: P]: P[String] = P("\\\"\"\"".!.map(_ => "\"\"\"") | sourceCharacter.!)

  private def stringValue[_: P]: P[StringValue] =
    P(
      ("\"\"\"" ~~ ((!"\"\"\"") ~~ blockStringCharacter).repX.map(s => blockStringValue(s.mkString)) ~~ "\"\"\"") |
        ("\"" ~~ stringCharacter.repX.map(_.mkString) ~~ "\"")
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

  private def nullValue[_: P]: P[InputValue] = P("null").map(_ => NullValue)
  private def enumValue[_: P]: P[InputValue] = P(name).map(EnumValue)
  private def listValue[_: P]: P[ListValue]  = P("[" ~/ value.rep ~ "]").map(values => ListValue(values.toList))

  private def objectField[_: P]: P[(String, InputValue)] = P(name ~ ":" ~/ value)
  private def objectValue[_: P]: P[ObjectValue] =
    P("{" ~ objectField.rep ~ "}").map(values => ObjectValue(values.toMap))

  private def value[_: P]: P[InputValue] =
    P(floatValue | intValue | booleanValue | stringValue | nullValue | enumValue | listValue | objectValue | variable)

  private def alias[_: P]: P[String] = P(name ~ ":")

  private def argument[_: P]: P[(String, InputValue)]     = P(name ~ ":" ~ value)
  private def arguments[_: P]: P[Map[String, InputValue]] = P("(" ~/ argument.rep ~ ")").map(_.toMap)

  private def directive[_: P]: P[Directive] = P(Index ~ "@" ~/ name ~ arguments).map {
    case (index, name, arguments) => Directive(name, arguments, index)
  }
  private def directives[_: P]: P[List[Directive]] = P(directive.rep).map(_.toList)

  private def selection[_: P]: P[Selection]          = P(field | fragmentSpread | inlineFragment)
  private def selectionSet[_: P]: P[List[Selection]] = P("{" ~/ selection.rep ~ "}").map(_.toList)

  private def namedType[_: P]: P[NamedType] = P(name.filter(_ != "null")).map(NamedType(_, nonNull = false))
  private def listType[_: P]: P[ListType]   = P("[" ~ type_ ~ "]").map(t => ListType(t, nonNull = false))

  private def argumentDefinition[_: P]: P[InputValueDefinition] =
    P(stringValue.? ~ name ~ ":" ~ type_ ~ defaultValue.? ~ directives.?).map {
      case (description, name, type_, defaultValue, directives) =>
        InputValueDefinition(description.map(_.value), name, type_, defaultValue, directives.getOrElse(Nil))
    }
  private def argumentDefinitions[_: P]: P[List[InputValueDefinition]] =
    P("(" ~/ argumentDefinition.rep ~ ")").map(_.toList)

  private def fieldDefinition[_: P]: P[FieldDefinition] =
    P(stringValue.? ~ name ~ argumentDefinitions.? ~ ":" ~ type_ ~ directives.?).map {
      case (description, name, args, type_, directives) =>
        FieldDefinition(description.map(_.value), name, args.getOrElse(Nil), type_, directives.getOrElse(Nil))
    }

  private def nonNullType[_: P]: P[Type] = P((namedType | listType) ~ "!").map {
    case t: NamedType => t.copy(nonNull = true)
    case t: ListType  => t.copy(nonNull = true)
  }
  private def type_[_: P]: P[Type] = P(nonNullType | namedType | listType)

  private def variable[_: P]: P[VariableValue] = P("$" ~/ name).map(VariableValue)
  private def variableDefinitions[_: P]: P[List[VariableDefinition]] =
    P("(" ~/ variableDefinition.rep ~ ")").map(_.toList)

  private def variableDefinition[_: P]: P[VariableDefinition] =
    P(variable ~ ":" ~/ type_ ~ defaultValue.? ~ directives).map {
      case (v, t, default, dirs) => VariableDefinition(v.name, t, default, dirs)
    }
  private def defaultValue[_: P]: P[InputValue] = P("=" ~/ value)

  private def field[_: P]: P[Field] = P(Index ~ alias.? ~ name ~ arguments.? ~ directives.? ~ selectionSet.?).map {
    case (index, alias, name, args, dirs, sels) =>
      Field(
        alias,
        name,
        args.getOrElse(Map()),
        dirs.getOrElse(Nil),
        sels.getOrElse(Nil),
        index
      )
  }

  private def fragmentName[_: P]: P[String] = P(name).filter(_ != "on")

  private def fragmentSpread[_: P]: P[FragmentSpread] = P("..." ~ fragmentName ~ directives).map {
    case (name, dirs) => FragmentSpread(name, dirs)
  }

  private def typeCondition[_: P]: P[NamedType] = P("on" ~/ namedType)

  private def inlineFragment[_: P]: P[InlineFragment] = P("..." ~ typeCondition.? ~ directives ~ selectionSet).map {
    case (typeCondition, dirs, sel) => InlineFragment(typeCondition, dirs, sel)
  }

  private def operationType[_: P]: P[OperationType] =
    P("query").map(_ => OperationType.Query) | P("mutation").map(_ => OperationType.Mutation) | P("subscription").map(
      _ => OperationType.Subscription
    )

  private def operationDefinition[_: P]: P[OperationDefinition] =
    P(operationType ~/ name.? ~ variableDefinitions.? ~ directives ~ selectionSet).map {
      case (operationType, name, variableDefinitions, directives, selection) =>
        OperationDefinition(operationType, name, variableDefinitions.getOrElse(Nil), directives, selection)
    } | P(selectionSet).map(selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection))

  private def fragmentDefinition[_: P]: P[FragmentDefinition] =
    P("fragment" ~/ fragmentName ~ typeCondition ~ directives ~ selectionSet).map {
      case (name, typeCondition, dirs, sel) => FragmentDefinition(name, typeCondition, dirs, sel)
    }

  private def objectTypeDefinition[_: P]: P[ObjectTypeDefinition] =
    P(stringValue.? ~ "type" ~/ name ~ directives.? ~ "{" ~ fieldDefinition.rep ~ "}").map {
      case (description, name, directives, fields) =>
        ObjectTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields.toList)
    }

  private def inputObjectTypeDefinition[_: P]: P[InputObjectTypeDefinition] =
    P(stringValue.? ~ "input" ~/ name ~ directives.? ~ "{" ~ argumentDefinition.rep ~ "}").map {
      case (description, name, directives, fields) =>
        InputObjectTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), fields.toList)
    }

  private def enumValueDefinition[_: P]: P[EnumValueDefinition] =
    P(stringValue.? ~ name ~ directives.?).map {
      case (description, enumValue, directives) =>
        EnumValueDefinition(description.map(_.value), enumValue, directives.getOrElse(Nil))
    }

  private def enumTypeDefinition[_: P]: P[EnumTypeDefinition] =
    P(
      stringValue.? ~ "enum" ~/ name
        .filter(s => s != "true" && s != "false" && s != "null") ~ directives.? ~ "{" ~ enumValueDefinition.rep ~ "}"
    ).map {
      case (description, name, directives, enumValuesDefinition) =>
        EnumTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), enumValuesDefinition.toList)
    }

  private def unionTypeDefinition[_: P]: P[UnionTypeDefinition] =
    P(stringValue.? ~ "union" ~/ name ~ directives.? ~ "=" ~ ("|".? ~ namedType) ~ ("|" ~ namedType).rep).map {
      case (description, name, directives, m, ms) =>
        UnionTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
    }

  private def scalarTypeDefinition[_: P]: P[ScalarTypeDefinition] =
    P(stringValue.? ~ "scalar" ~/ name ~ directives.?).map {
      case (description, name, directives) =>
        ScalarTypeDefinition(description.map(_.value), name, directives.getOrElse(Nil))
    }

  private def rootOperationTypeDefinition[_: P]: P[(OperationType, NamedType)] = P(operationType ~ ":" ~ namedType)

  private def schemaDefinition[_: P]: P[SchemaDefinition] =
    P("schema" ~/ directives.? ~ "{" ~ rootOperationTypeDefinition.rep ~ "}").map {
      case (directives, ops) =>
        val opsMap = ops.toMap
        SchemaDefinition(
          directives.getOrElse(Nil),
          opsMap.get(OperationType.Query).map(_.name),
          opsMap.get(OperationType.Mutation).map(_.name),
          opsMap.get(OperationType.Subscription).map(_.name)
        )
    }

  private def typeDefinition[_: P]: P[TypeDefinition] =
    objectTypeDefinition | inputObjectTypeDefinition | enumTypeDefinition | unionTypeDefinition | scalarTypeDefinition

  private def typeSystemDefinition[_: P]: P[TypeSystemDefinition] = typeDefinition | schemaDefinition

  private def executableDefinition[_: P]: P[ExecutableDefinition] =
    P(operationDefinition | fragmentDefinition)

  private def definition[_: P]: P[Definition] = executableDefinition | typeSystemDefinition

  private def document[_: P]: P[ParsedDocument] =
    P(Start ~ ignored ~ definition.rep ~ ignored ~ End).map(seq => ParsedDocument(seq.toList))

  /**
   * Parses the given string into a [[caliban.parsing.adt.Document]] object or fails with a [[caliban.CalibanError.ParsingError]].
   */
  def parseQuery(query: String): IO[ParsingError, Document] = {
    val sm = SourceMapper(query)
    Task(parse(query, document(_)))
      .mapError(ex => ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
      .flatMap {
        case Parsed.Success(value, _) => IO.succeed(Document(value.definitions, sm))
        case f: Parsed.Failure        => IO.fail(ParsingError(f.msg, Some(sm.getLocation(f.index))))
      }
  }

  /**
   * Checks if the query is valid, if not returns an error string.
   */
  def check(query: String): Option[String] = parse(query, document(_)) match {
    case Parsed.Success(_, _) => None
    case f: Parsed.Failure    => Some(f.msg)
  }
}

case class ParsedDocument(definitions: List[Definition], index: Int = 0)
