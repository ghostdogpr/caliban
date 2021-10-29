package caliban.tools

import Codegen.getPackageAndObjectName
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object CodegenSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("CodegenSpec")(
      test("Package name is empty or default if path doesn't follow correct format") {
        assert(packageAndObject("a/b/c", None, None))(
          equalTo(None -> "Client")
        ) &&
        assert(packageAndObject("a/b/c", Some("package"), None))(
          equalTo(Some("package") -> "Client")
        )
      },
      test("Package name is extracted correctly when set") {
        assert(packageAndObject("/a/b/c/src/main/scala/dirA/model.scala", Some("package"), None))(
          equalTo(Some("package") -> "model")
        )
      },
      test("Package name is extracted correctly absolute path") {
        assert(packageAndObject("/a/b/c/src/main/scala/dirA/model.scala", None, None))(
          equalTo(Some("dirA") -> "model")
        )
      },
      test("Package name is extracted correctly single level") {
        assert(packageAndObject("a/b/c/src/main/scala/dirA/model.scala", None, None))(
          equalTo(Some("dirA") -> "model")
        )
      },
      test("Package name is extracted correctly multiple levels") {
        assert(packageAndObject("a/b/c/src/main/scala/dirA/dirB/model.scala", None, None))(
          equalTo(Some("dirA.dirB") -> "model")
        )
      },
      test("Package name is extracted correctly for play and app") {
        assert(packageAndObject("app/dirA/dirB/model.scala", None, None))(
          equalTo(Some("dirA.dirB") -> "model")
        ) &&
        assert(packageAndObject("play/dirA/dirB/model.scala", None, None))(
          equalTo(Some("dirA.dirB") -> "model")
        ) &&
        assert(packageAndObject("play/src/main/scala/dirA/dirB/model.scala", None, None))(
          equalTo(Some("dirA.dirB") -> "model")
        )
      },
      test("Object name is extracted correctly when package is missing") {
        assert(packageAndObject("a/b/c/src/main/scala/model.scala", None, None))(
          equalTo(None -> "model")
        ) &&
        assert(packageAndObject("a/b/c/src/main/scala/model.scala", Some("package"), None))(
          equalTo(Some("package") -> "model")
        )
      },
      test("Object name is set to passed value") {
        assert(packageAndObject("src/main/scala/client.scala", None, Some("model")))(
          equalTo(None -> "model")
        ) &&
        assert(packageAndObject("", None, Some("model")))(
          equalTo(None -> "model")
        )
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
      extensibleEnums = None
    )

    getPackageAndObjectName(arguments)
  }
}
