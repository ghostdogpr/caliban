package caliban.interop.zio

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, ResponseValue, Value }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.parsing.adt.LocationInfo
import zio.Chunk
import zio.json.{ JsonDecoder, JsonEncoder }

import scala.annotation.switch

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsZIOJsonEncoder[F[_]]
private[caliban] object IsZIOJsonEncoder {
  implicit val isZIOJsonEncoder: IsZIOJsonEncoder[JsonEncoder] = null
}

private[caliban] trait IsZIOJsonDecoder[F[_]]
private[caliban] object IsZIOJsonDecoder {
  implicit val isZIOJsonDecoder: IsZIOJsonDecoder[JsonDecoder] = null
}

/**
 * Implementation derived from https://github.com/zio/zio-json/blob/develop/zio-json/shared/src/main/scala/zio/json/ast/ast.scala
 *
 * Apache License
 *                           Version 2.0, January 2004
 *                        http://www.apache.org/licenses/
 *
 *   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
 *
 *   1. Definitions.
 *
 *      "License" shall mean the terms and conditions for use, reproduction,
 *      and distribution as defined by Sections 1 through 9 of this document.
 *
 *      "Licensor" shall mean the copyright owner or entity authorized by
 *      the copyright owner that is granting the License.
 *
 *      "Legal Entity" shall mean the union of the acting entity and all
 *      other entities that control, are controlled by, or are under common
 *      control with that entity. For the purposes of this definition,
 *      "control" means (i) the power, direct or indirect, to cause the
 *      direction or management of such entity, whether by contract or
 *      otherwise, or (ii) ownership of fifty percent (50%) or more of the
 *      outstanding shares, or (iii) beneficial ownership of such entity.
 *
 *      "You" (or "Your") shall mean an individual or Legal Entity
 *      exercising permissions granted by this License.
 *
 *      "Source" form shall mean the preferred form for making modifications,
 *      including but not limited to software source code, documentation
 *      source, and configuration files.
 *
 *      "Object" form shall mean any form resulting from mechanical
 *      transformation or translation of a Source form, including but
 *      not limited to compiled object code, generated documentation,
 *      and conversions to other media types.
 *
 *      "Work" shall mean the work of authorship, whether in Source or
 *      Object form, made available under the License, as indicated by a
 *      copyright notice that is included in or attached to the work
 *      (an example is provided in the Appendix below).
 *
 *      "Derivative Works" shall mean any work, whether in Source or Object
 *      form, that is based on (or derived from) the Work and for which the
 *      editorial revisions, annotations, elaborations, or other modifications
 *      represent, as a whole, an original work of authorship. For the purposes
 *      of this License, Derivative Works shall not include works that remain
 *      separable from, or merely link (or bind by name) to the interfaces of,
 *      the Work and Derivative Works thereof.
 *
 *      "Contribution" shall mean any work of authorship, including
 *      the original version of the Work and any modifications or additions
 *      to that Work or Derivative Works thereof, that is intentionally
 *      submitted to Licensor for inclusion in the Work by the copyright owner
 *      or by an individual or Legal Entity authorized to submit on behalf of
 *      the copyright owner. For the purposes of this definition, "submitted"
 *      means any form of electronic, verbal, or written communication sent
 *      to the Licensor or its representatives, including but not limited to
 *      communication on electronic mailing lists, source code control systems,
 *      and issue tracking systems that are managed by, or on behalf of, the
 *      Licensor for the purpose of discussing and improving the Work, but
 *      excluding communication that is conspicuously marked or otherwise
 *      designated in writing by the copyright owner as "Not a Contribution."
 *
 *      "Contributor" shall mean Licensor and any individual or Legal Entity
 *      on behalf of whom a Contribution has been received by Licensor and
 *      subsequently incorporated within the Work.
 *
 *   2. Grant of Copyright License. Subject to the terms and conditions of
 *      this License, each Contributor hereby grants to You a perpetual,
 *      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
 *      copyright license to reproduce, prepare Derivative Works of,
 *      publicly display, publicly perform, sublicense, and distribute the
 *      Work and such Derivative Works in Source or Object form.
 *
 *   3. Grant of Patent License. Subject to the terms and conditions of
 *      this License, each Contributor hereby grants to You a perpetual,
 *      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
 *      (except as stated in this section) patent license to make, have made,
 *      use, offer to sell, sell, import, and otherwise transfer the Work,
 *      where such license applies only to those patent claims licensable
 *      by such Contributor that are necessarily infringed by their
 *      Contribution(s) alone or by combination of their Contribution(s)
 *      with the Work to which such Contribution(s) was submitted. If You
 *      institute patent litigation against any entity (including a
 *      cross-claim or counterclaim in a lawsuit) alleging that the Work
 *      or a Contribution incorporated within the Work constitutes direct
 *      or contributory patent infringement, then any patent licenses
 *      granted to You under this License for that Work shall terminate
 *      as of the date such litigation is filed.
 *
 *   4. Redistribution. You may reproduce and distribute copies of the
 *      Work or Derivative Works thereof in any medium, with or without
 *      modifications, and in Source or Object form, provided that You
 *      meet the following conditions:
 *
 *      (a) You must give any other recipients of the Work or
 *          Derivative Works a copy of this License; and
 *
 *      (b) You must cause any modified files to carry prominent notices
 *          stating that You changed the files; and
 *
 *      (c) You must retain, in the Source form of any Derivative Works
 *          that You distribute, all copyright, patent, trademark, and
 *          attribution notices from the Source form of the Work,
 *          excluding those notices that do not pertain to any part of
 *          the Derivative Works; and
 *
 *      (d) If the Work includes a "NOTICE" text file as part of its
 *          distribution, then any Derivative Works that You distribute must
 *          include a readable copy of the attribution notices contained
 *          within such NOTICE file, excluding those notices that do not
 *          pertain to any part of the Derivative Works, in at least one
 *          of the following places: within a NOTICE text file distributed
 *          as part of the Derivative Works; within the Source form or
 *          documentation, if provided along with the Derivative Works; or,
 *          within a display generated by the Derivative Works, if and
 *          wherever such third-party notices normally appear. The contents
 *          of the NOTICE file are for informational purposes only and
 *          do not modify the License. You may add Your own attribution
 *          notices within Derivative Works that You distribute, alongside
 *          or as an addendum to the NOTICE text from the Work, provided
 *          that such additional attribution notices cannot be construed
 *          as modifying the License.
 *
 *      You may add Your own copyright statement to Your modifications and
 *      may provide additional or different license terms and conditions
 *      for use, reproduction, or distribution of Your modifications, or
 *      for any such Derivative Works as a whole, provided Your use,
 *      reproduction, and distribution of the Work otherwise complies with
 *      the conditions stated in this License.
 *
 *   5. Submission of Contributions. Unless You explicitly state otherwise,
 *      any Contribution intentionally submitted for inclusion in the Work
 *      by You to the Licensor shall be under the terms and conditions of
 *      this License, without any additional terms or conditions.
 *      Notwithstanding the above, nothing herein shall supersede or modify
 *      the terms of any separate license agreement you may have executed
 *      with Licensor regarding such Contributions.
 *
 *   6. Trademarks. This License does not grant permission to use the trade
 *      names, trademarks, service marks, or product names of the Licensor,
 *      except as required for reasonable and customary use in describing the
 *      origin of the Work and reproducing the content of the NOTICE file.
 *
 *   7. Disclaimer of Warranty. Unless required by applicable law or
 *      agreed to in writing, Licensor provides the Work (and each
 *      Contributor provides its Contributions) on an "AS IS" BASIS,
 *      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 *      implied, including, without limitation, any warranties or conditions
 *      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
 *      PARTICULAR PURPOSE. You are solely responsible for determining the
 *      appropriateness of using or redistributing the Work and assume any
 *      risks associated with Your exercise of permissions under this License.
 *
 *   8. Limitation of Liability. In no event and under no legal theory,
 *      whether in tort (including negligence), contract, or otherwise,
 *      unless required by applicable law (such as deliberate and grossly
 *      negligent acts) or agreed to in writing, shall any Contributor be
 *      liable to You for damages, including any direct, indirect, special,
 *      incidental, or consequential damages of any character arising as a
 *      result of this License or out of the use or inability to use the
 *      Work (including but not limited to damages for loss of goodwill,
 *      work stoppage, computer failure or malfunction, or any and all
 *      other commercial damages or losses), even if such Contributor
 *      has been advised of the possibility of such damages.
 *
 *   9. Accepting Warranty or Additional Liability. While redistributing
 *      the Work or Derivative Works thereof, You may choose to offer,
 *      and charge a fee for, acceptance of support, warranty, indemnity,
 *      or other liability obligations and/or rights consistent with this
 *      License. However, in accepting such obligations, You may act only
 *      on Your own behalf and on Your sole responsibility, not on behalf
 *      of any other Contributor, and only if You agree to indemnify,
 *      defend, and hold each Contributor harmless for any liability
 *      incurred by, or claims asserted against, such Contributor by reason
 *      of your accepting any such warranty or additional liability.
 *
 *   END OF TERMS AND CONDITIONS
 *
 *   APPENDIX: How to apply the Apache License to your work.
 *
 *      To apply the Apache License to your work, attach the following
 *      boilerplate notice, with the fields enclosed by brackets "[]"
 *      replaced with your own identifying information. (Don't include
 *      the brackets!)  The text should be enclosed in the appropriate
 *      comment syntax for the file format. We also recommend that a
 *      file or class name and description of purpose be included on the
 *      same "printed page" as the copyright notice for easier
 *      identification within third-party archives.
 *
 *   Copyright [yyyy] [name of copyright owner]
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */
private[caliban] object ValueZIOJson {
  import zio.json._
  import zio.json.internal.{ Lexer, RetractReader, Write }

  val valueEncoder: JsonEncoder[Value] = (a: Value, indent: Option[Int], out: Write) =>
    a match {
      case Value.NullValue     => Null.encoder.unsafeEncode(NullValue, indent, out)
      case v: IntValue         =>
        v match {
          case IntValue.IntNumber(value)    => JsonEncoder.int.unsafeEncode(value, indent, out)
          case IntValue.LongNumber(value)   => JsonEncoder.long.unsafeEncode(value, indent, out)
          case IntValue.BigIntNumber(value) => JsonEncoder.bigInteger.unsafeEncode(value.bigInteger, indent, out)
        }
      case v: FloatValue       =>
        v match {
          case FloatValue.FloatNumber(value)      => JsonEncoder.float.unsafeEncode(value, indent, out)
          case FloatValue.DoubleNumber(value)     => JsonEncoder.double.unsafeEncode(value, indent, out)
          case FloatValue.BigDecimalNumber(value) => JsonEncoder.bigDecimal.unsafeEncode(value.bigDecimal, indent, out)
        }
      case StringValue(value)  => JsonEncoder.string.unsafeEncode(value, indent, out)
      case BooleanValue(value) => JsonEncoder.boolean.unsafeEncode(value, indent, out)
      case EnumValue(value)    => JsonEncoder.string.unsafeEncode(value, indent, out)
    }

  object Null {
    private[this] val nullChars: Array[Char]          = "null".toCharArray
    val encoder: JsonEncoder[NullValue.type]          = (a: NullValue.type, indent: Option[Int], out: Write) => out.write("null")
    implicit val decoder: JsonDecoder[NullValue.type] =
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
        Lexer.readChars(trace, in, nullChars, "null")
        NullValue
      }
  }

  object Bool {
    implicit val decoder: JsonDecoder[BooleanValue] = JsonDecoder.boolean.map(BooleanValue)
  }

  object Obj {
    val decoder: JsonDecoder[InputValue.ObjectValue] = {
      val objd = JsonDecoder.keyValueChunk[String, InputValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) =>
        InputValue.ObjectValue(objd.unsafeDecode(trace, in).toMap)
    }

    val responseDecoder: JsonDecoder[ResponseValue.ObjectValue] = {
      val objd = JsonDecoder.keyValueChunk[String, ResponseValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) =>
        ResponseValue.ObjectValue(objd.unsafeDecode(trace, in).toList)
    }

    val responseEncoder: JsonEncoder[List[(String, ResponseValue)]] = {
      val obje = JsonEncoder.keyValueChunk[String, ResponseValue]
      (a: List[(String, ResponseValue)], indent: Option[Int], out: Write) =>
        obje.unsafeEncode(Chunk.fromIterable(a), indent, out)
    }
  }

  object Arr {
    val decoder: JsonDecoder[InputValue.ListValue] = {
      val arrd = JsonDecoder.list[InputValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => InputValue.ListValue(arrd.unsafeDecode(trace, in))
    }

    val responseDecoder: JsonDecoder[ResponseValue.ListValue] = {
      val arrd = JsonDecoder.list[ResponseValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => ResponseValue.ListValue(arrd.unsafeDecode(trace, in))
    }

    val responseEncoder: JsonEncoder[List[ResponseValue]] =
      JsonEncoder.list[ResponseValue]

  }

  object Str {
    val decoder: JsonDecoder[StringValue] = JsonDecoder.string.map(StringValue)
  }

  object Num {
    val decoder: JsonDecoder[Value] = (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
      val bd = BigDecimal(JsonDecoder.bigDecimal.unsafeDecode(trace, in))
      if (bd.isValidInt) IntValue(bd.toIntExact)
      else if (bd.isValidLong) IntValue(bd.toLongExact)
      else bd.toBigIntExact.fold[Value](FloatValue(bd))(IntValue(_))
    }
  }

  implicit val inputValueDecoder: JsonDecoder[InputValue] = (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
    val c = in.nextNonWhitespace()
    in.retract()
    (c: @switch) match {
      case 'n'                                                             => Null.decoder.unsafeDecode(trace, in)
      case 'f' | 't'                                                       => Bool.decoder.unsafeDecode(trace, in)
      case '{'                                                             => Obj.decoder.unsafeDecode(trace, in)
      case '['                                                             => Arr.decoder.unsafeDecode(trace, in)
      case '"'                                                             => Str.decoder.unsafeDecode(trace, in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        Num.decoder.unsafeDecode(trace, in)
      case c                                                               =>
        throw JsonDecoder.UnsafeJson(JsonDecoder.JsonError.Message(s"unexpected '$c'") :: trace)
    }

  }

  val inputValueEncoder: JsonEncoder[InputValue] = (a: InputValue, indent: Option[Int], out: Write) =>
    a match {
      case value: Value                   => valueEncoder.unsafeEncode(value, indent, out)
      case InputValue.ListValue(values)   => JsonEncoder.list(inputValueEncoder).unsafeEncode(values, indent, out)
      case InputValue.ObjectValue(fields) =>
        JsonEncoder.map(JsonFieldEncoder.string, inputValueEncoder).unsafeEncode(fields, indent, out)
      case InputValue.VariableValue(name) => JsonEncoder.string.unsafeEncode(name, indent, out)
    }

  val responseValueDecoder: JsonDecoder[ResponseValue] = (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
    val c = in.nextNonWhitespace()
    in.retract()
    (c: @switch) match {
      case 'n'                                                             => Null.decoder.unsafeDecode(trace, in)
      case 'f' | 't'                                                       => Bool.decoder.unsafeDecode(trace, in)
      case '{'                                                             => Obj.responseDecoder.unsafeDecode(trace, in)
      case '['                                                             => Arr.responseDecoder.unsafeDecode(trace, in)
      case '"'                                                             => Str.decoder.unsafeDecode(trace, in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        Num.decoder.unsafeDecode(trace, in)
      case c                                                               =>
        throw JsonDecoder.UnsafeJson(JsonDecoder.JsonError.Message(s"unexpected '$c'") :: trace)
    }

  }

  implicit val responseValueEncoder: JsonEncoder[ResponseValue] =
    (a: ResponseValue, indent: Option[Int], out: Write) =>
      a match {
        case value: Value                      => valueEncoder.unsafeEncode(value, indent, out)
        case ResponseValue.ListValue(values)   => Arr.responseEncoder.unsafeEncode(values, indent, out)
        case ResponseValue.ObjectValue(fields) => Obj.responseEncoder.unsafeEncode(fields, indent, out)
        case s: ResponseValue.StreamValue      => JsonEncoder.string.unsafeEncode(s.toString, indent, out)
      }

}

private[caliban] object ErrorZioJson {
  import zio.json._
  import zio.json.internal.Write

  private def locationToResponse(li: LocationInfo): ResponseValue =
    ResponseValue.ListValue(
      List(ResponseValue.ObjectValue(List("line" -> IntValue(li.line), "column" -> IntValue(li.column))))
    )

  private[caliban] def errorToResponseValue(e: CalibanError): ResponseValue =
    ResponseValue.ObjectValue(e match {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions)         =>
        List(
          "message"                                               -> StringValue(s"Parsing Error: $msg")
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations" -> _)
      case CalibanError.ValidationError(msg, _, locationInfo, extensions)      =>
        List(
          "message"                                               -> StringValue(msg)
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations" -> _)
      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        List(
          "message"                                               -> StringValue(msg)
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations" -> _) ++
          Some(path).collect {
            case p if p.nonEmpty =>
              "path" -> ResponseValue.ListValue(p.map {
                case Left(value)  => StringValue(value)
                case Right(value) => IntValue(value)
              })
          }
    })

  val errorValueEncoder: JsonEncoder[CalibanError] = (a: CalibanError, indent: Option[Int], out: Write) =>
    ValueZIOJson.responseValueEncoder.unsafeEncode(
      errorToResponseValue(a),
      indent,
      out
    )
}

private[caliban] object GraphQLResponseZioJson {
  import zio.json._
  import zio.json.internal.Write

  private def handleError(err: Any): ResponseValue =
    err match {
      case ce: CalibanError => ErrorZioJson.errorToResponseValue(ce)
      case _                => ResponseValue.ObjectValue(List("message" -> StringValue(err.toString)))
    }

  val graphQLResponseEncoder: JsonEncoder[GraphQLResponse[Any]] =
    (a: GraphQLResponse[Any], indent: Option[Int], out: Write) => {
      val responseEncoder = JsonEncoder.map[String, ResponseValue]
      a match {
        case GraphQLResponse(data, Nil, None)                =>
          responseEncoder.unsafeEncode(Map("data" -> data), indent, out)
        case GraphQLResponse(data, Nil, Some(extensions))    =>
          responseEncoder.unsafeEncode(
            Map("data" -> data, "extension" -> extensions.asInstanceOf[ResponseValue]),
            indent,
            out
          )
        case GraphQLResponse(data, errors, None)             =>
          responseEncoder.unsafeEncode(
            Map("data" -> data, "errors" -> ResponseValue.ListValue(errors.map(handleError))),
            indent,
            out
          )
        case GraphQLResponse(data, errors, Some(extensions)) =>
          responseEncoder.unsafeEncode(
            Map(
              "data"       -> data,
              "errors"     -> ResponseValue.ListValue(errors.map(handleError)),
              "extensions" -> extensions.asInstanceOf[ResponseValue]
            ),
            indent,
            out
          )
      }
    }
}

private[caliban] object GraphQLRequestZioJson {
  import zio.json._

  val graphQLRequestDecoder: JsonDecoder[GraphQLRequest] = DeriveJsonDecoder.gen[GraphQLRequest]
}
