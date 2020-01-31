package caliban

import play.api.libs.json.{ JsObject, Json, Reads, Writes }

object playCaliban extends ValuePlay with LowPrioPlayJsonImplicits {
  implicit val playWrites: Writes[ResponseValue] =
    responseValueWrites

  implicit val playReads: Reads[InputValue] =
    inputValueReads

  implicit def playResponseWrites[E: Writes]: Writes[GraphQLResponse[E]] = Writes {
    case GraphQLResponse(data, Nil)    => Json.obj("data" -> data)
    case GraphQLResponse(data, errors) => Json.obj("data" -> data, "errors" -> errors)
  }
}

trait LowPrioPlayJsonImplicits {
  implicit val writes: Writes[CalibanError] =
    Writes.of[JsObject].contramap[CalibanError] { error =>
      Json.obj("message" -> error.toString)
    }

  implicit val throwableWrites: Writes[Throwable] =
    Writes.of[JsObject].contramap[Throwable] { error =>
      Json.obj("message" -> error.toString)
    }
}
