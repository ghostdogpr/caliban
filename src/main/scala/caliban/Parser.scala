package caliban

import caliban.Parser.Type._
import caliban.Parser.Value._
import fastparse._

object Parser {

  def sourceCharacter[_: P]: P[Unit]                 = P(CharIn("\u0009\u000A\u000D\u0020-\uFFFF"))
  def sourceCharacterNoLineTerminator[_: P]: P[Unit] = P(CharIn("\u0009\u0020-\uFFFF"))

  def unicodeBOM[_: P]: P[Unit]     = P("\uFEFF")
  def whiteSpace[_: P]: P[Unit]     = P(CharIn("\u0009\u0020"))
  def lineTerminator[_: P]: P[Unit] = P("\u000A" | "\u000D" ~~ !"\u000A" | "\u000D\u000A")
  def comma[_: P]: P[Unit]          = P(",")
  def commentChar[_: P]: P[Unit]    = P(sourceCharacterNoLineTerminator)
  def comment[_: P]: P[Unit]        = P("#" ~~ commentChar.repX)
  def ignored[_: P]: P[Unit]        = P(unicodeBOM | whiteSpace | lineTerminator | comment | comma).repX

  implicit val whitespace: P[_] => P[Unit] = { implicit ctx: ParsingRun[_] =>
    ignored
  }

  def name[_: P]: P[String] = P(CharIn("_A-Za-z") ~~ CharIn("_0-9A-Za-z").repX).!

  def booleanValue[_: P]: P[BooleanValue] =
    P("true").map(_ => BooleanValue(true)) | P("false").map(_ => BooleanValue(false))

  def negativeSign[_: P]: P[Unit] = P("-")
  def nonZeroDigit[_: P]: P[Unit] = P(CharIn("1-9"))
  def digit[_: P]: P[Unit]        = P("0" | nonZeroDigit)
  def integerPart[_: P]: P[Unit]  = P((negativeSign.? ~~ "0") | (negativeSign.? ~~ nonZeroDigit ~~ digit.repX))
  def intValue[_: P]: P[IntValue] = integerPart.!.map(v => IntValue(v.toInt))

  def sign[_: P]: P[Unit]              = P("-" | "+")
  def exponentIndicator[_: P]: P[Unit] = P(CharIn("eE"))
  def exponentPart[_: P]: P[Unit]      = P(exponentIndicator ~~ sign.? ~~ digit.repX(1))
  def fractionalPart[_: P]: P[Unit]    = P("." ~~ digit.repX(1))

  def floatValue[_: P]: P[FloatValue] =
    P((integerPart ~~ fractionalPart) | (integerPart ~~ exponentPart) | (integerPart ~~ fractionalPart ~~ exponentPart)).!.map(
      v => FloatValue(v.toFloat)
    )

  def hexDigit[_: P]: P[Unit] = P(CharIn("0-9a-fA-F"))
  def escapedUnicode[_: P]: P[String] =
    P(hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit).!.map(Integer.parseInt(_, 16).toChar.toString)

  def escapedCharacter[_: P]: P[String] = P(CharIn("\"\\/bfnrt").!).map {
    case "b"   => "\b"
    case "n"   => "\n"
    case "f"   => "\f"
    case "r"   => "\r"
    case "t"   => "\t"
    case other => other
  }

  def stringCharacter[_: P]: P[String] =
    P(
      sourceCharacterNoLineTerminator.!.filter(c => c != "\"" && c != "\\") | "\\u" ~~ escapedUnicode | "\\" ~~ escapedCharacter
    )

  def blockStringCharacter[_: P]: P[String] = P("\\\"\"\"".!.map(_ => "\"\"\"") | sourceCharacter.!)

  def stringValue[_: P]: P[StringValue] =
    P(
      ("\"\"\"" ~~ ((!"\"\"\"") ~~ blockStringCharacter).repX.map(s => blockStringValue(s.mkString)) ~~ "\"\"\"") |
        ("\"" ~~ stringCharacter.repX.map(_.mkString) ~~ "\"")
    ).map(v => StringValue(v))

  def blockStringValue(rawValue: String): String = {
    val l1 = rawValue.split("\r?\n").toList
    val commonIndent = l1 match {
      case Nil => None
      case _ :: tail =>
        tail.foldLeft(Option.empty[Int]) {
          case (commonIndent, line) =>
            val indent = "[ \t]*".r.findPrefixOf(line).map(_.length).getOrElse(0)
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

  def nullValue[_: P]: P[Value]     = P("null").map(_ => NullValue)
  def enumValue[_: P]: P[Value]     = P(name).map(EnumValue)
  def listValue[_: P]: P[ListValue] = P("[" ~ value.rep ~ "]").map(values => ListValue(values.toList))

  def objectField[_: P]: P[(String, Value)] = P(name ~ ":" ~ value)
  def objectValue[_: P]: P[ObjectValue]     = P("{" ~ objectField.rep ~ "}").map(values => ObjectValue(values.toMap))

  def value[_: P]: P[Value] =
    P(floatValue | intValue | booleanValue | stringValue | nullValue | enumValue | listValue | objectValue | variable)

  def alias[_: P]: P[String] = P(name ~ ":")

  def argument[_: P]: P[(String, Value)]     = P(name ~ ":" ~ value)
  def arguments[_: P]: P[Map[String, Value]] = P("(" ~ argument.rep ~ ")").map(_.toMap)

  def directive[_: P]: P[(String, Map[String, Value])]        = P("@" ~ name ~ arguments)
  def directives[_: P]: P[List[(String, Map[String, Value])]] = P(directive.rep).map(_.toList)

  def selection[_: P]: P[Selection]          = P(field | fragmentSpread | inlineFragment)
  def selectionSet[_: P]: P[List[Selection]] = P("{" ~ selection.rep ~ "}").map(_.toList)

  def namedType[_: P]: P[NamedType] = P(name.filter(_ != "null")).map(NamedType(_, nonNull = false))
  def listType[_: P]: P[ListType]   = P("[" ~ type_.rep ~ "]").map(t => ListType(t.toList, nonNull = false))

  def nonNullType[_: P]: P[Type] = P((namedType | listType) ~ "!").map {
    case t: NamedType => t.copy(nonNull = true)
    case t: ListType  => t.copy(nonNull = true)
  }
  def type_[_: P]: P[Type] = P(nonNullType | namedType | listType)

  def variable[_: P]: P[VariableValue]                       = P("$" ~ name).map(VariableValue)
  def variableDefinitions[_: P]: P[List[VariableDefinition]] = P("(" ~ variableDefinition.rep ~ ")").map(_.toList)

  def variableDefinition[_: P]: P[VariableDefinition] = P(variable ~ ":" ~ type_ ~ defaultValue.? ~ directives).map {
    case (v, t, default, dirs) => VariableDefinition(v.name, t, default, dirs)
  }
  def defaultValue[_: P]: P[Value] = P("=" ~ value)

  def field[_: P]: P[Field] = P(alias.? ~ name ~ arguments.? ~ directives.? ~ selectionSet.?).map {
    case (alias, name, args, dirs, sels) =>
      Field(
        alias,
        name,
        args.getOrElse(Map()),
        dirs.getOrElse(Nil),
        sels.getOrElse(Nil)
      )
  }

  def fragmentName[_: P]: P[String] = P(name).filter(_ != "on")

  def fragmentSpread[_: P]: P[FragmentSpread] = P("..." ~ fragmentName ~ directives).map {
    case (name, dirs) => FragmentSpread(name, dirs)
  }

  def typeCondition[_: P]: P[NamedType] = P("on" ~ namedType)

  def inlineFragment[_: P]: P[InlineFragment] = P("..." ~ typeCondition.? ~ directives ~ selectionSet).map {
    case (typeCondition, dirs, sel) => InlineFragment(typeCondition, dirs, sel)
  }

  def operationType[_: P]: P[OperationType] =
    P("query").map(_ => OperationType.Query) | P("mutation").map(_ => OperationType.Mutation) | P("subscription").map(
      _ => OperationType.Subscription
    )

  def operationDefinition[_: P]: P[OperationDefinition] =
    P(operationType ~ name.? ~ variableDefinitions.? ~ directives ~ selectionSet).map {
      case (operationType, name, variableDefinitions, directives, selection) =>
        OperationDefinition(operationType, name, variableDefinitions.getOrElse(Nil), directives, selection)
    } | P(selectionSet).map(selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection))

  def fragmentDefinition[_: P]: P[FragmentDefinition] =
    P("fragment" ~ fragmentName ~ typeCondition ~ directives ~ selectionSet).map {
      case (name, typeCondition, dirs, sel) => FragmentDefinition(name, typeCondition, dirs, sel)
    }

  def executableDefinition[_: P] = P(operationDefinition | fragmentDefinition)

  def definition[_: P] = executableDefinition

  def document[_: P] = P(Start ~ ignored ~ definition.rep ~ ignored ~ End)

  def temp[_: P] = P(ignored ~ operationDefinition ~ ignored)

  sealed trait Selection

  case class Field(
    alias: Option[String],
    name: String,
    arguments: Map[String, Value],
    directives: List[(String, Map[String, Value])],
    selectionSet: List[Selection]
  ) extends Selection

  case class FragmentSpread(name: String, directives: List[(String, Map[String, Value])]) extends Selection

  case class InlineFragment(
    typeCondition: Option[NamedType],
    dirs: List[(String, Map[String, Value])],
    selectionSet: List[Selection]
  ) extends Selection

  case class VariableDefinition(
    name: String,
    variableType: Type,
    defaultValue: Option[Value],
    directives: List[(String, Map[String, Value])]
  )

  case class OperationDefinition(
    operationType: OperationType,
    name: Option[String],
    variableDefinitions: List[VariableDefinition],
    directives: List[(String, Map[String, Value])],
    selectionSet: List[Selection]
  )

  case class FragmentDefinition(
    name: String,
    typeCondition: NamedType,
    directives: List[(String, Map[String, Value])],
    selectionSet: List[Selection]
  )

  sealed trait Value

  object Value {
    case object NullValue                              extends Value
    case class IntValue(value: Int)                    extends Value
    case class FloatValue(value: Float)                extends Value
    case class StringValue(value: String)              extends Value
    case class BooleanValue(value: Boolean)            extends Value
    case class EnumValue(value: String)                extends Value
    case class ListValue(values: List[Value])          extends Value
    case class ObjectValue(fields: Map[String, Value]) extends Value
    case class VariableValue(name: String)             extends Value
  }

  sealed trait Type

  object Type {
    case class NamedType(name: String, nonNull: Boolean)     extends Type
    case class ListType(types: List[Type], nonNull: Boolean) extends Type
  }

  sealed trait OperationType

  object OperationType {
    case object Query        extends OperationType
    case object Mutation     extends OperationType
    case object Subscription extends OperationType
  }

  val query =
    """
    # some comment
    query getZuckProfile($devicePicSize: Int! = 2){
      someAlias: endpoint(width: 100, height: 50.0, test: "ok\t\u0042", empty:"", n: null, enum: TOTO, list: [1,2 "3"]) @skip(if: true) {
        me {
          id
          nearestThing(location: { lat: -53.211, lon: 12.43 })
          pic(size: $devicePicSize)
          firstName
          lastName
          birthday {
            month
            day
            ...f
          }
          friends {
            name
          }
          ... on Page {
            likers {
            count
          }
    }
        }
      }
    }
    fragment f on User {
      id
      name
      ...standardProfilePic
    }
    fragment standardProfilePic on User {
      profilePic(size: 50)
    }
  
  """

  val query2 = "{ sendEmail(message: \"\"\"\n  Hello,\n    World!\n\n  Yours,\n    GraphQL. \\\"\"\"\n \"\"\") }"

  val query3 =
    """
      query GetDeity {
        deity (name: "Morpheus", city: Athens) {
          fullName
          power
          city
        }
      }
      """

  def parseQuery(query: String): Parsed[OperationDefinition] = parse(query, temp(_))

//  val res = parse(query, document(_))
//  println(res)
//  val res2 = parse(query2, document(_))
//  println(res2)
//  val res3 = parse(query3, temp(_))
//  println(res3)
}
