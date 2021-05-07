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
import cats.parse.{ Numbers, Parser => P }
import cats.parse._
import zio.{ IO, Task }

object Parser {
  private final val UnicodeBOM                           = '\uFEFF'
  private final val Tab                                  = '\u0009'
  private final val Space                                = '\u0020'
  private final val LF                                   = '\u000A'
  private final val CR                                   = '\u000D'
  private final val Comma                                = ','
  private val whitespace: Parser[_]                      = P.charIn(UnicodeBOM, Tab, Space, LF, CR, Comma)
  private val comment: Parser[_]                         = P.charIn('#') ~ P.until(P.char(LF) | P.string(s"$CR$LF"))
  private val whitespaceWithComment                      = (whitespace | comment).rep0.void
  private val whitespaceWithComment1                     = (whitespace | comment).rep.void
  private def wrapBrackets[T](t: Parser0[T]): P[T]       =
    P.char('{') *> whitespaceWithComment *> t <* whitespaceWithComment <* P.char('}')
  private def wrapParentheses[T](t: Parser0[T]): P[T]    =
    P.char('(') *> whitespaceWithComment *> t <* whitespaceWithComment <* P.char(')')
  private def wrapSquareBrackets[T](t: Parser0[T]): P[T] =
    P.char('[').surroundedBy(whitespaceWithComment) *> t <* (P.char(']').surroundedBy(whitespaceWithComment))
  private def wrapWhitespaces[T](t: Parser[T]): P[T]     = t.surroundedBy(whitespaceWithComment)

  private object StringUtil {
    private val decodeTable: Map[Char, Char] = Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar),  // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )

    val escapedToken: P[Unit] = {
      val escapes = P.charIn(decodeTable.keys.toSeq)

      val oct  = P.charIn('0' to '7')
      val octP = P.char('o') ~ oct ~ oct

      val hex  = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
      val hex2 = hex ~ hex
      val hexP = P.char('x') ~ hex2

      val hex4 = hex2 ~ hex2
      val u4   = P.char('u') ~ hex4
      val hex8 = hex4 ~ hex4
      val u8   = P.char('U') ~ hex8

      val after = P.oneOf[Any](escapes :: octP :: hexP :: u4 :: u8 :: Nil)
      (P.char('\\') ~ after).void
    }

    /**
     * String content without the delimiter
     */
    def undelimitedString(endP: P[Unit]): P[String] =
      escapedToken.backtrack
        .orElse((!endP).with1 ~ P.anyChar)
        .rep
        .string
        .flatMap { str =>
          unescape(str) match {
            case Right(str1) => P.pure(str1)
            case Left(_)     => P.fail
          }
        }

    private val simpleString: Parser0[String] =
      P.charsWhile0(c => c >= ' ' && c != '"' && c != '\\')

    def escapedString(q: Char): P[String] = {
      val end: P[Unit] = P.char(q)
      end *> ((simpleString <* end).backtrack
        .orElse(undelimitedString(end) <* end))
    }

    def unescape(str: String): Either[Int, String] = {
      val sb                  = new java.lang.StringBuilder
      def decodeNum(idx: Int, size: Int, base: Int): Int = {
        val end = idx + size
        if (end <= str.length) {
          val intStr = str.substring(idx, end)
          val asInt  =
            try Integer.parseInt(intStr, base)
            catch { case _: NumberFormatException => ~idx }
          sb.append(asInt.toChar)
          end
        } else ~(str.length)
      }
      @annotation.tailrec
      def loop(idx: Int): Int =
        if (idx >= str.length) {
          // done
          idx
        } else if (idx < 0) {
          // error from decodeNum
          idx
        } else {
          val c0 = str.charAt(idx)
          if (c0 != '\\') {
            sb.append(c0)
            loop(idx + 1)
          } else {
            // str(idx) == \
            val nextIdx = idx + 1
            if (nextIdx >= str.length) {
              // error we expect there to be a character after \
              ~idx
            } else {
              val c = str.charAt(nextIdx)
              decodeTable.get(c) match {
                case Some(d) =>
                  sb.append(d)
                  loop(idx + 2)
                case None    =>
                  c match {
                    case 'o'   => loop(decodeNum(idx + 2, 2, 8))
                    case 'x'   => loop(decodeNum(idx + 2, 2, 16))
                    case 'u'   => loop(decodeNum(idx + 2, 4, 16))
                    case 'U'   => loop(decodeNum(idx + 2, 8, 16))
                    case other =>
                      // \c is interpretted as just \c, if the character isn't escaped
                      sb.append('\\')
                      sb.append(other)
                      loop(idx + 2)
                  }
              }
            }
          }
        }

      val res = loop(0)
      if (res < 0) Left(~res)
      else Right(sb.toString)
    }
  }

  private val name: P[String] = (P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ Seq('_')) ~ P
    .charIn(('a' to 'z') ++ ('A' to 'Z') ++ Seq('_') ++ ('0' to '9'))
    .rep0).string

  private val booleanValue: P[BooleanValue] =
    P.oneOf(P.string("true").as(BooleanValue(true)) :: P.string("false").as(BooleanValue(false)) :: Nil)

  private val intValue: P[IntValue] = (Numbers.signedIntString <* (!P.char('.')).void).backtrack.map(IntValue(_))

  private val floatValue: P[FloatValue] = Numbers.jsonNumber.map(FloatValue(_))

  private val stringValue: P[StringValue] =
    P.oneOf(
      (P.string("\"\"\"") *> StringUtil.undelimitedString(P.string("\"\"\"")).map(blockStringValue) <*
        P.string("\"\"\"")) :: StringUtil.escapedString('\"') :: Nil
    ).map(v => StringValue(v))

  private def blockStringValue(rawValue: String): String = {
    val l1           = rawValue.split("\r?\n").toList
    val commonIndent = l1 match {
      case Nil       => None
      case _ :: tail =>
        tail.foldLeft(Option.empty[Int]) { case (commonIndent, line) =>
          val indent = "[ \t]*".r.findPrefixOf(line).fold(0)(_.length)
          if (indent < line.length && commonIndent.fold(true)(_ > indent)) Some(indent) else commonIndent
        }
    }
    // remove indentation
    val l2           = (commonIndent, l1) match {
      case (Some(value), head :: tail) => head :: tail.map(_.drop(value))
      case _                           => l1
    }
    // remove start lines that are only whitespaces
    val l3           = l2.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty)
    // remove end lines that are only whitespaces
    val l4           = l3.reverse.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty).reverse
    l4.mkString("\n")
  }

  private val nullValue: P[InputValue] = P.string("null").as(NullValue)
  private val enumValue: P[InputValue] = name.map(EnumValue.apply)

  private val listValue: P[ListValue] =
    wrapSquareBrackets(value.repSep0(whitespaceWithComment)).map(values => ListValue(values))

  private val objectField: P[(String, InputValue)] = (name <* wrapWhitespaces(P.char(':'))) ~ value

  private val objectValue: P[ObjectValue] =
    wrapBrackets(objectField.repSep0(whitespaceWithComment)).map(values => ObjectValue(values.toMap))

  private val variable: P[VariableValue] = (P.char('$') *> name).map(VariableValue.apply)

  private lazy val value: P[InputValue] =
    P.defer(
      P.oneOf(
        List(intValue, floatValue, booleanValue, stringValue, nullValue, enumValue, listValue, objectValue, variable)
      )
    )

  private val defaultValue: P[InputValue] = wrapWhitespaces(P.char('=')) *> value

  private val alias: P[String] = name <* whitespaceWithComment <* P.char(':')

  private val argument: P[(String, InputValue)]     = (name <* wrapWhitespaces(P.char(':'))) ~ value
  private val arguments: P[Map[String, InputValue]] =
    wrapParentheses(argument.repSep0(whitespaceWithComment)).map(v => v.toMap)

  private val directive: P[Directive]        =
    (P.index.with1 ~ ((P.char('@') *> name).soft <* whitespaceWithComment) ~ arguments.?).map {
      case ((index, name), arguments) =>
        Directive(name, arguments.getOrElse(Map()), index)
    }
  private val directives: P[List[Directive]] = directive.repSep(whitespaceWithComment).map(_.toList)

  private val selection: P[Selection] = P.defer(P.oneOf(field :: fragmentSpread :: inlineFragment :: Nil))

  private lazy val selectionSet: P[List[Selection]] =
    wrapBrackets(selection.repSep0(whitespaceWithComment)).map(_.toList)

  private val namedType: P[NamedType] = (name.filter(_ != "null") ~ P.char('!').?).map { case (name, nonNull) =>
    NamedType(name, nonNull = nonNull.nonEmpty)
  }

  private val listType: P[ListType] =
    (wrapSquareBrackets(type_) ~ P.char('!').?).map { case (typ, nonNull) => ListType(typ, nonNull = nonNull.nonEmpty) }

  private lazy val type_ : P[Type]  = P.defer(P.oneOf(namedType :: listType :: Nil))

  private val argumentDefinition: P[InputValueDefinition]        =
    (((stringValue <* whitespaceWithComment1).?.with1 ~ name <* wrapWhitespaces(P.char(':'))) ~
      (type_ <* whitespaceWithComment) ~ ((defaultValue <* whitespaceWithComment).? ~ directives.?)).map {
      case (((description, name), type_), (defaultValue, directives)) =>
        InputValueDefinition(description.map(_.value), name, type_, defaultValue, directives.getOrElse(Nil))
    }
  private val argumentDefinitions: P[List[InputValueDefinition]] =
    wrapParentheses(argumentDefinition.rep).map(_.toList)

  private val fieldDefinition: P[FieldDefinition] =
    (((stringValue <* whitespaceWithComment).?.with1 ~ (name <* whitespaceWithComment)) ~
      (argumentDefinitions <* whitespaceWithComment).? ~
      ((P.char(':').void <* whitespaceWithComment) *> type_ <* whitespaceWithComment) ~ directives.?).map {
      case ((((description, name), args), type_), directives) =>
        FieldDefinition(description.map(_.value), name, args.getOrElse(Nil), type_, directives.getOrElse(Nil))
    }

  private val variableDefinition: P[VariableDefinition] =
    ((variable <* wrapWhitespaces(P.char(':'))) ~
      (type_ <* whitespaceWithComment) ~
      ((defaultValue <* whitespaceWithComment).? ~ directives.?)).map { case ((v, t), (default, dirs)) =>
      VariableDefinition(v.name, t, default, dirs.getOrElse(Nil))
    }

  private val variableDefinitions: P[List[VariableDefinition]] =
    wrapParentheses(variableDefinition.repSep0(whitespaceWithComment))

  private val field: P[Field] = (((P.index ~ (alias <* whitespaceWithComment).backtrack.?).soft.with1 ~
    name <* whitespaceWithComment) ~ (arguments <* whitespaceWithComment).? ~
    (directives <* whitespaceWithComment).? ~ selectionSet.?).map {
    case (((((index, alias), name), args), dirs), sels) =>
      Field(
        alias,
        name,
        args.getOrElse(Map()),
        dirs.getOrElse(Nil),
        sels.getOrElse(Nil),
        index
      )
  }

  private val fragmentName: P[String] = name.filter(_ != "on")

  private val fragmentSpread: P[FragmentSpread] =
    ((P.string("...").soft *> fragmentName <* whitespaceWithComment) ~ directives.?).map { case (name, dirs) =>
      FragmentSpread(name, dirs.getOrElse(Nil))
    }

  private val typeCondition: P[NamedType] = P.string("on") *> whitespaceWithComment1 *> namedType

  private val inlineFragment: P[InlineFragment] = (P.string("...") *> whitespaceWithComment *>
    (typeCondition <* whitespaceWithComment).? ~ (directives <* whitespaceWithComment).? ~ selectionSet).map {
    case ((typeCondition, dirs), sel) =>
      InlineFragment(typeCondition, dirs.getOrElse(Nil), sel)
  }

  private val operationType: P[OperationType] =
    P.string("query").as(OperationType.Query) | P.string("mutation").as(OperationType.Mutation) | P
      .string("subscription")
      .as(
        OperationType.Subscription
      )

  private val operationDefinition: P[OperationDefinition] =
    P.oneOf(
      ((operationType <* whitespaceWithComment) ~ ((name <* whitespaceWithComment).? ~
        (variableDefinitions <* whitespaceWithComment).?) ~
        (directives <* whitespaceWithComment).? ~ selectionSet).map {
        case (((operationType, (name, variableDefinitions)), directives), selection) =>
          OperationDefinition(
            operationType,
            name,
            variableDefinitions.getOrElse(Nil),
            directives.getOrElse(Nil),
            selection
          )
      } :: selectionSet
        .map(selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection)) :: Nil
    )

  private val fragmentDefinition: P[FragmentDefinition] =
    ((P.string("fragment").void *> whitespaceWithComment1 *> fragmentName <* whitespaceWithComment1) ~
      (typeCondition <* whitespaceWithComment) ~ (directives <* whitespaceWithComment).? ~ selectionSet).map {
      case (((name, typeCondition), dirs), sel) =>
        FragmentDefinition(name, typeCondition, dirs.getOrElse(Nil), sel)
    }

  private def objectTypeDefinition(description: Option[String]): P[ObjectTypeDefinition] =
    ((P.string("type").void *> whitespaceWithComment1 *> name <* whitespaceWithComment1) ~
      ((implements <* whitespaceWithComment).? ~ (directives <* whitespaceWithComment).?) ~
      wrapBrackets(fieldDefinition.repSep0(whitespaceWithComment))).map {
      case ((name, (implements, directives)), fields) =>
        ObjectTypeDefinition(
          description,
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields
        )
    }

  private val implements: P[List[NamedType]] = (((P.string("implements") <* whitespaceWithComment <*
    (P.char('&') <* whitespaceWithComment).?) *> namedType <* whitespaceWithComment) ~
    (P.char('&') *> whitespaceWithComment *> namedType).repSep0(whitespaceWithComment)).map { case (head, tail) =>
    head :: tail
  }

  private def interfaceTypeDefinition(description: Option[String]): P[InterfaceTypeDefinition] =
    ((P.string("interface") *> whitespaceWithComment1 *> name <* whitespaceWithComment) ~
      (directives <* whitespaceWithComment).? ~ wrapBrackets(
      fieldDefinition.repSep0(whitespaceWithComment)
    )).map { case ((name, directives), fields) =>
      InterfaceTypeDefinition(description, name, directives.getOrElse(Nil), fields)
    }

  private def inputObjectTypeDefinition(description: Option[String]): P[InputObjectTypeDefinition] =
    ((P.string(
      "input"
    ) *> whitespaceWithComment1 *> name <* whitespaceWithComment) ~ (directives <* whitespaceWithComment).? ~
      wrapBrackets(argumentDefinition.repSep0(whitespaceWithComment))).map { case ((name, directives), fields) =>
      InputObjectTypeDefinition(description, name, directives.getOrElse(Nil), fields)
    }

  private val enumValueDefinition: P[EnumValueDefinition] =
    ((stringValue <* whitespaceWithComment).?.with1 ~ (name <* whitespaceWithComment) ~ directives.?).map {
      case ((description, enumValue), directives) =>
        EnumValueDefinition(description.map(_.value), enumValue, directives.getOrElse(Nil))
    }

  private val enumName: P[String] = name.filter(s => s != "true" && s != "false" && s != "null")

  private def enumTypeDefinition(description: Option[String]): P[EnumTypeDefinition] =
    ((P.string("enum") *> whitespaceWithComment1 *> enumName <* whitespaceWithComment) ~
      (directives <* whitespaceWithComment).? ~ wrapBrackets(
      enumValueDefinition.repSep0(whitespaceWithComment)
    )).map { case ((name, directives), enumValuesDefinition) =>
      EnumTypeDefinition(description, name, directives.getOrElse(Nil), enumValuesDefinition)
    }

  private def unionTypeDefinition(description: Option[String]): P[UnionTypeDefinition] =
    ((P.string("union") *> whitespaceWithComment1 *> name <* whitespaceWithComment) ~
      ((directives <* whitespaceWithComment).? <* P.char('=') <* whitespaceWithComment) ~
      ((P.char('|') <* whitespaceWithComment).? *> namedType <* whitespaceWithComment) ~
      ((P.char('|') <* whitespaceWithComment) *> namedType).repSep(whitespaceWithComment)).map {
      case (((name, directives), m), ms) =>
        UnionTypeDefinition(description, name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
    }

  private def scalarTypeDefinition(description: Option[String]): P[ScalarTypeDefinition] =
    ((P.string("scalar") *> whitespaceWithComment1 *> name <* whitespaceWithComment) ~ directives.?).map {
      case (name, directives) =>
        ScalarTypeDefinition(description, name, directives.getOrElse(Nil))
    }

  private val rootOperationTypeDefinition: P[(OperationType, NamedType)] =
    (operationType <* wrapWhitespaces(P.char(':'))) ~ namedType

  private val schemaDefinition: P[SchemaDefinition] =
    ((P.string("schema") *> whitespaceWithComment *> (directives <* whitespaceWithComment).?).with1 ~
      wrapBrackets(rootOperationTypeDefinition.repSep0(whitespaceWithComment))).map { case (directives, ops) =>
      val opsMap = ops.toMap
      SchemaDefinition(
        directives.getOrElse(Nil),
        opsMap.get(OperationType.Query).map(_.name),
        opsMap.get(OperationType.Mutation).map(_.name),
        opsMap.get(OperationType.Subscription).map(_.name)
      )
    }

  private val schemaExtensionWithOptionalDirectivesAndOperations: Parser0[SchemaExtension] =
    ((directives <* whitespaceWithComment).? ~
      wrapBrackets(rootOperationTypeDefinition.repSep0(whitespaceWithComment)).?).map { case (directives, ops) =>
      val opsMap = ops.getOrElse(Nil).toMap
      SchemaExtension(
        directives.getOrElse(Nil),
        opsMap.get(OperationType.Query).map(_.name),
        opsMap.get(OperationType.Mutation).map(_.name),
        opsMap.get(OperationType.Subscription).map(_.name)
      )
    }

  private val schemaExtension: P[SchemaExtension] =
    P.string("schema") *> whitespaceWithComment *> schemaExtensionWithOptionalDirectivesAndOperations

  private val scalarTypeExtension: P[ScalarTypeExtension] =
    ((P.string("scalar") *> whitespaceWithComment *> name <* whitespaceWithComment) ~ directives).map {
      case (name, directives) =>
        ScalarTypeExtension(name, directives)
    }

  private val objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields: P[ObjectTypeExtension] =
    ((name <* whitespaceWithComment) ~ ((implements <* whitespaceWithComment).? ~
      (directives <* whitespaceWithComment).?) ~
      wrapBrackets(fieldDefinition.repSep0(whitespaceWithComment)).backtrack.?).map {
      case ((name, (implements, directives)), fields) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields.getOrElse(Nil)
        )
    }

  private val objectTypeExtension: P[ObjectTypeExtension] =
    P.string("type") *> whitespaceWithComment1 *>
      objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields

  private val interfaceTypeExtensionWithOptionalDirectivesAndFields: P[InterfaceTypeExtension] =
    ((name <* whitespaceWithComment) ~ ((directives <* whitespaceWithComment).? ~
      wrapBrackets(fieldDefinition.repSep0(whitespaceWithComment)).?)).map { case (name, (directives, fields)) =>
      InterfaceTypeExtension(name, directives.getOrElse(Nil), fields.getOrElse(Nil))
    }

  private val interfaceTypeExtension: P[InterfaceTypeExtension] =
    P.string("interface") *> whitespaceWithComment1 *>
      interfaceTypeExtensionWithOptionalDirectivesAndFields

  private val unionTypeExtensionWithOptionalDirectivesAndUnionMembers: P[UnionTypeExtension] =
    ((name <* whitespaceWithComment) ~
      ((directives <* whitespaceWithComment).? <* (P.char('=') <* whitespaceWithComment).?) ~
      ((P.char('|') <* whitespaceWithComment).? *> (namedType <* whitespaceWithComment).?) ~
      ((P.char('|') <* whitespaceWithComment) *> namedType).repSep0(whitespaceWithComment)).map {
      case (((name, directives), m), ms) =>
        UnionTypeExtension(name, directives.getOrElse(Nil), m.map(_ :: ms).getOrElse(ms).map(_.name))
    }

  private val unionTypeExtension: P[UnionTypeExtension] =
    P.string("union") *> whitespaceWithComment1 *>
      unionTypeExtensionWithOptionalDirectivesAndUnionMembers

  private val enumTypeExtensionWithOptionalDirectivesAndValues: P[EnumTypeExtension] =
    ((enumName <* whitespaceWithComment) ~ (directives <* whitespaceWithComment).? ~
      wrapBrackets(enumValueDefinition.repSep0(whitespaceWithComment)).backtrack.?).map {
      case ((name, directives), enumValuesDefinition) =>
        EnumTypeExtension(name, directives.getOrElse(Nil), enumValuesDefinition.getOrElse(Nil))
    }

  private val enumTypeExtension: P[EnumTypeExtension] =
    P.string("enum") *> whitespaceWithComment1 *> enumTypeExtensionWithOptionalDirectivesAndValues

  private val inputObjectTypeExtensionWithOptionalDirectivesAndFields: P[InputObjectTypeExtension] =
    ((name <* whitespaceWithComment) ~ (directives <* whitespaceWithComment).? ~
      wrapBrackets(argumentDefinition.repSep0(whitespaceWithComment)).?).map { case ((name, directives), fields) =>
      InputObjectTypeExtension(name, directives.getOrElse(Nil), fields.getOrElse(Nil))
    }

  private val inputObjectTypeExtension: P[InputObjectTypeExtension] =
    P.string("input") *> whitespaceWithComment1 *>
      inputObjectTypeExtensionWithOptionalDirectivesAndFields

  private val directiveLocation: P[DirectiveLocation] =
    P.oneOf(
      List(
        P.string("QUERY").as(ExecutableDirectiveLocation.QUERY),
        P.string("MUTATION").as(ExecutableDirectiveLocation.MUTATION),
        P.string("SUBSCRIPTION").as(ExecutableDirectiveLocation.SUBSCRIPTION),
        P.string("FIELD").as(ExecutableDirectiveLocation.FIELD),
        P.string("FRAGMENT_DEFINITION").as(ExecutableDirectiveLocation.FRAGMENT_DEFINITION),
        P.string("FRAGMENT_SPREAD").as(ExecutableDirectiveLocation.FRAGMENT_SPREAD),
        P.string("INLINE_FRAGMENT").as(ExecutableDirectiveLocation.INLINE_FRAGMENT),
        P.string("SCHEMA").as(TypeSystemDirectiveLocation.SCHEMA),
        P.string("SCALAR").as(TypeSystemDirectiveLocation.SCALAR),
        P.string("OBJECT").as(TypeSystemDirectiveLocation.OBJECT),
        P.string("FIELD_DEFINITION").as(TypeSystemDirectiveLocation.FIELD_DEFINITION),
        P.string("ARGUMENT_DEFINITION").as(TypeSystemDirectiveLocation.ARGUMENT_DEFINITION),
        P.string("INTERFACE").as(TypeSystemDirectiveLocation.INTERFACE),
        P.string("UNION").as(TypeSystemDirectiveLocation.UNION),
        P.string("ENUM").as(TypeSystemDirectiveLocation.ENUM),
        P.string("ENUM_VALUE").as(TypeSystemDirectiveLocation.ENUM_VALUE),
        P.string("INPUT_OBJECT").as(TypeSystemDirectiveLocation.INPUT_OBJECT),
        P.string("INPUT_FIELD_DEFINITION").as(TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION)
      )
    )

  private val directiveDefinition: P[DirectiveDefinition] =
    ((stringValue <* whitespaceWithComment).?.with1 ~
      (P.string("directive @") *> name <* whitespaceWithComment) ~
      ((argumentDefinitions <* whitespaceWithComment).? <* P.string("on") <* whitespaceWithComment1) ~
      ((P.char('|') <* whitespaceWithComment).? *> directiveLocation <* whitespaceWithComment) ~
      (P.char('|') *> whitespaceWithComment *> directiveLocation).repSep(whitespaceWithComment)).map {
      case ((((description, name), args), firstLoc), otherLoc) =>
        DirectiveDefinition(description.map(_.value), name, args.getOrElse(Nil), otherLoc.toList.toSet + firstLoc)
    }

  private val typeDefinition: P[TypeDefinition] =
    (stringValue <* whitespaceWithComment).?.with1.flatMap { stringValOpt =>
      val description = stringValOpt.map(_.value)
      P.oneOf(
        objectTypeDefinition(description) ::
          interfaceTypeDefinition(description) ::
          inputObjectTypeDefinition(description) ::
          enumTypeDefinition(description) ::
          unionTypeDefinition(description) ::
          scalarTypeDefinition(description) :: Nil
      )
    }

  private val typeSystemDefinition: P[TypeSystemDefinition] =
    P.oneOf(typeDefinition :: schemaDefinition :: directiveDefinition :: Nil)

  private val executableDefinition: P[ExecutableDefinition] =
    P.oneOf(operationDefinition :: fragmentDefinition :: Nil)

  private val typeExtension: P[TypeExtension] =
    P.oneOf(
      objectTypeExtension ::
        interfaceTypeExtension ::
        inputObjectTypeExtension ::
        enumTypeExtension ::
        unionTypeExtension ::
        scalarTypeExtension :: Nil
    )

  private val typeSystemExtension: P[TypeSystemExtension] =
    P.string("extend ").void *> P.oneOf(schemaExtension :: typeExtension :: Nil)

  private def definition: P[Definition] =
    P.oneOf(executableDefinition :: typeSystemDefinition :: typeSystemExtension :: Nil)

  private val document: Parser0[ParsedDocument] =
    (P.start *> whitespaceWithComment *> definition.repSep0(whitespaceWithComment) <* whitespaceWithComment <* P.end)
      .map(seq => ParsedDocument(seq))

  /**
   * Parses the given string into a [[caliban.parsing.adt.Document]] object or fails with a [[caliban.CalibanError.ParsingError]].
   */
  def parseQuery(query: String): IO[ParsingError, Document] = {
    val sm = SourceMapper(query)
    //    document.parse(query) match {
    //      case Left(error) =>
    //        IO.fail(ParsingError(error.toString, Some(sm.getLocation(error.failedAtOffset))))
    //      case Right(result) =>
    //        IO.succeed(Document(result._2.definitions,sm))
    //    }
    Task(document.parse(query))
      .mapError(ex => ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
      .flatMap {
        case Left(error)   =>
          IO.fail(ParsingError(error.toString, Some(sm.getLocation(error.failedAtOffset))))
        case Right(result) =>
          IO.succeed(Document(result._2.definitions, sm))
      }
  }

  /**
   * Checks if the query is valid, if not returns an error string.
   */
  def check(query: String): Option[String] = document.parse(query) match {
    case Left(error) => Some(error.toString)
    case Right(_)    => None
  }
}

case class ParsedDocument(definitions: List[Definition], index: Int = 0)
