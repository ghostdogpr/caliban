package caliban.gateway.internal

import caliban.parsing.adt.Document
import caliban.rendering.DocumentRenderer

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
 * Fingerprints the rendered schema, so source locations and formatting do not change it.
 * The original document is never modified.
 */
private[gateway] object SchemaFingerprint {
  def apply(document: Document): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(DocumentRenderer.render(document).getBytes(StandardCharsets.UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
}
