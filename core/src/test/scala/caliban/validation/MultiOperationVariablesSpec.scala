package caliban.validation

import caliban._
import caliban.CalibanError.ValidationError
import caliban.Value.StringValue
import caliban.parsing.VariablesCoercer
import caliban.parsing.Parser
import caliban.schema.RootType
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import zio._
import zio.test._
import zio.test.Assertion._

/**
 * Tests for multi-operation documents where each operation has different required variables.
 *
 * Per the GraphQL spec, variables are scoped on a per-operation basis:
 * "Variables are scoped on a per-operation basis. That means that any variable
 * used within the context of an operation must be defined at the top level of
 * that operation."
 *
 * When a document contains multiple named operations and one is selected via
 * operationName, only the variables defined by that operation should be coerced.
 *
 * This is a common scenario when GraphQL clients (e.g., code generators) bundle multiple
 * operations into a single document. Users should be able to execute any individual
 * operation without providing variables for unrelated operations in the same document.
 *
 * @see https://spec.graphql.org/October2021/#sec-All-Variable-Uses-Defined
 * @see https://spec.graphql.org/October2021/#sec-Executing-Requests
 */
object MultiOperationVariablesSpec extends ZIOSpecDefault {

  // Simple API for testing - represents any two-step workflow with different required inputs
  case class Queries(dummy: UIO[String])
  case class Mutations(
    sendEmail: String => UIO[Boolean],
    verifyToken: String => UIO[Boolean]
  )

  val queries   = Queries(dummy = ZIO.succeed("ok"))
  val mutations = Mutations(
    sendEmail = _ => ZIO.succeed(true),
    verifyToken = _ => ZIO.succeed(true)
  )

  val api = graphQL(RootResolver(queries, mutations))

  // Get the RootType for direct coercion testing
  val rootType: RootType = api.validateRootSchema match {
    case Right(schema) =>
      RootType(
        schema.query.opType,
        Some(schema.mutation.get.opType),
        None,
        Nil,
        Nil,
        None
      )
    case Left(e)       => throw new RuntimeException(s"Schema validation failed: $e")
  }

  // Document with two mutations, each with its own required variable
  val multiOperationDocument: String =
    """
    mutation SendEmail($email: String!) {
      sendEmail(value: $email)
    }

    mutation VerifyToken($token: String!) {
      verifyToken(value: $token)
    }
    """

  val parsedDoc = Parser.parseQuery(multiOperationDocument).toOption.get

  override def spec = suite("Multi-Operation Variable Coercion")(
    suite("VariablesCoercer with operationName")(
      test("coercing variables for SendEmail should only require $email") {
        // When operationName is "SendEmail", only $email should be required
        val variables = Map("email" -> StringValue("test@example.com"))
        val result    = VariablesCoercer.coerceVariables(
          variables,
          parsedDoc,
          rootType,
          skipValidation = false,
          operationName = Some("SendEmail")
        )
        assertTrue(result.isRight)
      },
      test("coercing variables for VerifyToken should only require $token") {
        // When operationName is "VerifyToken", only $token should be required
        val variables = Map("token" -> StringValue("abc123"))
        val result    = VariablesCoercer.coerceVariables(
          variables,
          parsedDoc,
          rootType,
          skipValidation = false,
          operationName = Some("VerifyToken")
        )
        assertTrue(result.isRight)
      },
      test("coercing SendEmail without $email should fail") {
        // Missing required variable should still fail
        val variables = Map.empty[String, InputValue]
        val result    = VariablesCoercer.coerceVariables(
          variables,
          parsedDoc,
          rootType,
          skipValidation = false,
          operationName = Some("SendEmail")
        )
        assertTrue(result.isLeft) &&
        assertTrue(result.left.toOption.exists(_.msg.contains("email")))
      },
      test("coercing SendEmail should not require $token from VerifyToken operation") {
        // This is the key test: $token is required by VerifyToken, not SendEmail.
        // When executing SendEmail, missing $token should NOT cause an error.
        val variables = Map("email" -> StringValue("test@example.com"))
        // Note: we're NOT providing $token
        val result    = VariablesCoercer.coerceVariables(
          variables,
          parsedDoc,
          rootType,
          skipValidation = false,
          operationName = Some("SendEmail")
        )

        // With the fix: this should succeed (only $email is checked)
        // Without the fix: this fails with "Variable 'token' is null but is specified to be non-null"
        val hasTokenError = result.left.toOption.exists(_.msg.toLowerCase.contains("token"))

        assertTrue(!hasTokenError) &&
        assertTrue(result.isRight)
      },
      test("coercing VerifyToken should not require $email from SendEmail operation") {
        // Symmetrical test: $email is required by SendEmail, not VerifyToken
        val variables = Map("token" -> StringValue("abc123"))
        // Note: we're NOT providing $email
        val result    = VariablesCoercer.coerceVariables(
          variables,
          parsedDoc,
          rootType,
          skipValidation = false,
          operationName = Some("VerifyToken")
        )

        val hasEmailError = result.left.toOption.exists(_.msg.toLowerCase.contains("email"))

        assertTrue(!hasEmailError) &&
        assertTrue(result.isRight)
      }
    ),
    suite("VariablesCoercer without operationName")(
      test("single operation document - coercion works without operationName") {
        val singleOpDoc = Parser
          .parseQuery("""
          mutation SendEmail($email: String!) {
            sendEmail(value: $email)
          }
        """)
          .toOption
          .get

        val variables = Map("email" -> StringValue("test@example.com"))
        val result    = VariablesCoercer.coerceVariables(
          variables,
          singleOpDoc,
          rootType,
          skipValidation = false,
          operationName = None
        )
        assertTrue(result.isRight)
      },
      test("multi-operation document without operationName - falls back to checking all variables") {
        // When no operationName is provided and there are multiple operations,
        // the coercer checks all variable definitions (legacy behavior)
        val variables = Map("email" -> StringValue("test@example.com"))
        val result    = VariablesCoercer.coerceVariables(
          variables,
          parsedDoc,
          rootType,
          skipValidation = false,
          operationName = None
        )
        // This should fail because $token is not provided
        assertTrue(result.isLeft)
      }
    ),

    // ==========================================================================
    // End-to-end tests using the full interpreter execution path
    // These tests demonstrate the complete fix working through all layers.
    // ==========================================================================
    suite("End-to-end execution with multi-operation documents")(
      test("executing SendEmail with only $email should succeed") {
        // This test exercises the full execution path: parsing -> coercion -> validation -> execution
        for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(
                           multiOperationDocument,
                           operationName = Some("SendEmail"),
                           variables = Map("email" -> StringValue("test@example.com"))
                         )
        } yield assertTrue(result.errors.isEmpty) &&
          assertTrue(result.data.toString.contains("true"))
      },
      test("executing VerifyToken with only $token should succeed") {
        // Symmetrical test: execute VerifyToken without providing $email
        for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(
                           multiOperationDocument,
                           operationName = Some("VerifyToken"),
                           variables = Map("token" -> StringValue("abc123"))
                         )
        } yield assertTrue(result.errors.isEmpty) &&
          assertTrue(result.data.toString.contains("true"))
      },
      test("executing SendEmail without $email should still fail") {
        // Required variable for the SELECTED operation must still be validated
        for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(
                           multiOperationDocument,
                           operationName = Some("SendEmail"),
                           variables = Map.empty[String, InputValue]
                         )
        } yield assertTrue(result.errors.nonEmpty) &&
          assertTrue(result.errors.exists(e => e.msg.toLowerCase.contains("email")))
      },
      test("executing VerifyToken without $token should still fail") {
        // Required variable for the SELECTED operation must still be validated
        for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(
                           multiOperationDocument,
                           operationName = Some("VerifyToken"),
                           variables = Map.empty[String, InputValue]
                         )
        } yield assertTrue(result.errors.nonEmpty) &&
          assertTrue(result.errors.exists(e => e.msg.toLowerCase.contains("token")))
      },
      test("single operation document executes without operationName") {
        // Control case: single operation documents should work as before
        val singleOpDoc = """
          mutation SendEmail($email: String!) {
            sendEmail(value: $email)
          }
        """
        for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(
                           singleOpDoc,
                           variables = Map("email" -> StringValue("test@example.com"))
                         )
        } yield assertTrue(result.errors.isEmpty)
      }
    )
  )
}
