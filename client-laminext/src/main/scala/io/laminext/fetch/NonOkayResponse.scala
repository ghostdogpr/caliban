package io.laminext.fetch

import org.scalajs.dom.Response

class NonOkayResponse(val response: Response) extends Throwable
