package caliban.tools

import Codegen.getPackageAndObjectName
import zio.test._

object CodegenSpec extends ZIOSpecDefault {

  override def spec =
    suite("CodegenSpec")(
      test("Package name is empty or default if path doesn't follow correct format") {
        assertTrue(packageAndObject("a/b/c", None, None) == None -> "Client") &&
        assertTrue(packageAndObject("a/b/c", Some("package"), None) == Some("package") -> "Client")
      },
      test("Package name is extracted correctly when set") {
        assertTrue(
          packageAndObject("/a/b/c/src/main/scala/dirA/model.scala", Some("package"), None) == Some(
            "package"
          ) -> "model"
        )
      },
      test("Package name is extracted correctly absolute path") {
        assertTrue(packageAndObject("/a/b/c/src/main/scala/dirA/model.scala", None, None) == Some("dirA") -> "model")
      },
      test("Package name is extracted correctly single level") {
        assertTrue(packageAndObject("a/b/c/src/main/scala/dirA/model.scala", None, None) == Some("dirA") -> "model")
      },
      test("Package name is extracted correctly multiple levels") {
        assertTrue(
          packageAndObject("a/b/c/src/main/scala/dirA/dirB/model.scala", None, None) == Some("dirA.dirB") -> "model"
        )
      },
      test("Package name is extracted correctly for play and app") {
        assertTrue(packageAndObject("app/dirA/dirB/model.scala", None, None) == Some("dirA.dirB") -> "model") &&
        assertTrue(packageAndObject("play/dirA/dirB/model.scala", None, None) == Some("dirA.dirB") -> "model") &&
        assertTrue(
          packageAndObject("play/src/main/scala/dirA/dirB/model.scala", None, None) == Some("dirA.dirB") -> "model"
        )
      },
      test("Object name is extracted correctly when package is missing") {
        assertTrue(packageAndObject("a/b/c/src/main/scala/model.scala", None, None) == None -> "model") &&
        assertTrue(
          packageAndObject("a/b/c/src/main/scala/model.scala", Some("package"), None) == Some("package") -> "model"
        )
      },
      test("Object name is set to passed value") {
        assertTrue(packageAndObject("src/main/scala/client.scala", None, Some("model")) == None -> "model") &&
        assertTrue(packageAndObject("", None, Some("model")) == None -> "model")
      }
    )

  def packageAndObject(toPath: String, defaultPackageName: Option[String], defaultClientName: Option[String]) = {
    val arguments = Options(
      schemaPath = "schema.graphql",
      toPath = toPath,
      fmtPath = None,
      headers = None,
      packageName = defaultPackageName,
      clientName = defaultClientName,
      genView = None,
      effect = None,
      scalarMappings = None,
      imports = None,
      abstractEffectType = None,
      splitFiles = None,
      enableFmt = None,
      extensibleEnums = None,
      preserveInputNames = None,
      supportIsRepeatable = None,
      addDerives = None
    )

    getPackageAndObjectName(arguments)
  }
}
