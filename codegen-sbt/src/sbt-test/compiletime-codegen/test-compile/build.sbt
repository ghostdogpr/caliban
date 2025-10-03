import sbt.Def.spaceDelimited
import sbt.librarymanagement.Resolver

val scala212 = "2.12.20"
val scala213 = "2.13.18"
val scala3   = "3.3.7"
val allScala = Seq(scala212, scala213, scala3)

def scalaDefaultVersion(sbtVersion: String): String =
  (sys.props.get("plugin.version"), sbtVersion) match {
    case (Some("test-codegen-sbt-compile-scala3"), _) => scala3
    case (_, v) if v.startsWith("2.")                 => scala3
    case _                                            => scala212
  }

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization       := "Conduktor"
ThisBuild / homepage           := Some(url("https://www.conduktor.io/"))
ThisBuild / licenses           := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / version            := "0.0.1"
ThisBuild / scalaVersion       := scalaDefaultVersion(sbtVersion.value)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / scalacOptions ~= (opts => (opts ++ Seq("-Xfatal-warnings", "-feature")).distinct)
ThisBuild / crossScalaVersions := allScala

// ### Dependencies ###

lazy val calibanLib = Seq(
  "com.github.ghostdogpr" %% "caliban"         % Version.pluginVersion,
  "com.github.ghostdogpr" %% "caliban-codegen" % Version.pluginVersion % "compile->compile;test->test"
)

lazy val sttp = Seq(
  "com.softwaremill.sttp.client4" %% "core" % "4.0.2",
  "com.softwaremill.sttp.client4" %% "zio"  % "4.0.2"
)

lazy val zioTest = Seq(
  "dev.zio" %% "zio-test"     % "2.1.9" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.9" % Test
)

// ### sbt scripted helpers ###
def absent(file: File): Unit = if (file.exists()) throw new MessageOnlyException(s"File exists: $file")
def exists(file: File): Unit = if (!file.exists()) throw new MessageOnlyException(s"File does not exist: $file")

lazy val copyableFiles = taskKey[Map[String, (File, File)]]("Copyable files")

// ### App Modules ###

/**
 * `root` is a "meta module". It's the "main module" of this project but doesn't have a physical existence. It represents the "current
 * project" if you prefer, composed of modules.
 *
 * The `aggregate` setting will instruct sbt that when you're launching an sbt command, you want it applied to all the aggregated modules
 */
lazy val root =
  Project(id = "poc_compile_time_caliban_client_generation", base = file("."))
    .aggregate(
      posts,
      potatoes,
      clients,
      postsClients,
      potatoesClients
    )
    .settings(
      copyableFiles := {
        val postsTarget = (posts / target).value
        val potatoesTarget = (potatoes / target).value
        val generatedPostsDir = (postsClients / sourceManaged).value / "main" / "poc" / "caliban" / "client" / "generated" / "posts"
        val generatedPotatoesDir = (potatoesClients / Compile / sourceDirectory).value / "scala" / "poc" / "caliban" / "client" / "generated" / "potatoes"

        Map(
          "postsTouch" -> (postsTarget / "ctCalibanServer" / "touch", postsTarget / "ctCalibanServer" / "touch_old"),
          "potatoesTouch" -> (potatoesTarget / "ctCalibanServer" / "touch", potatoesTarget / "ctCalibanServer" / "touch_old"),
          "postsCalibanClient" -> (generatedPostsDir / "CalibanClient.scala", generatedPostsDir / "CalibanClient.scala_old"),
          "potatoesPackage" -> (generatedPotatoesDir / "package.scala", generatedPotatoesDir / "package.scala_old"),
        )
      },
      // Additional scripted tests commands
      InputKey[Unit]("copy-file-with-options") := {
        val args: Vector[String] = spaceDelimited("<arg>").parsed.toVector

        IO.copy(
          List(copyableFiles.value(args(3))),
          overwrite = args(0).toBoolean,
          preserveLastModified = args(1).toBoolean,
          preserveExecutable = args(2).toBoolean
        )
      },
      InputKey[Unit]("check-file-newer") := {
        val args: Vector[String] = spaceDelimited("<arg>").parsed.toVector

        val swap = args(0).toBoolean
        val files = copyableFiles.value(args(1))
        val (fileA, fileB) = if (swap) files.swap else files
        val isNewer = fileA.exists &&
          (!fileB.exists || IO.getModifiedTimeOrZero(fileA) > IO.getModifiedTimeOrZero(fileB))

        if (!isNewer) throw new MessageOnlyException(s"$fileA is not newer than $fileB")
      },
      InputKey[Unit]("sed-in-place")           := {
        val args: Vector[String] = spaceDelimited("<arg>").parsed.toVector

        val previousValue = args(0)
        val newValue      = args(1)
        val baseDir       = baseDirectory.value.getAbsolutePath
        val initialFile   = s"$baseDir/${args(2)}"
        val backupFile    = s"$baseDir/${args(2)}.old"

        IO.move(file(initialFile), file(backupFile))
        val content    = IO.read(file(backupFile))
        val newContent = content.replace(previousValue, newValue)
        IO.write(file(initialFile), newContent)
      },
      TaskKey[Unit]("check-generated-files-pre-compile") := {
        val generatedPostsDir = (postsClients / sourceManaged).value / "main" / "poc" / "caliban" / "client" / "generated" / "posts"
        val generatedPotatoesDir = (potatoesClients / Compile / sourceDirectory).value / "scala" / "poc" / "caliban" / "client" / "generated" / "potatoes"

        // From the 'posts' "default" config
        absent((postsClients / sourceManaged).value / "main" / "generated" / "Client.scala")

        // From the 'posts' "CalibanClient" config
        absent(generatedPostsDir / "CalibanClient.scala")

        // From the 'posts' "split" config
        absent(generatedPostsDir / "split" / "package.scala")
        absent(generatedPostsDir / "split" / "AuthorName.scala")
        absent(generatedPostsDir / "split" / "AuthorNameInput.scala")
        absent(generatedPostsDir / "split" / "Mutation.scala")
        absent(generatedPostsDir / "split" / "Post.scala")
        absent(generatedPostsDir / "split" / "PostContent.scala")
        absent(generatedPostsDir / "split" / "PostContentInput.scala")
        absent(generatedPostsDir / "split" / "PostId.scala")
        absent(generatedPostsDir / "split" / "PostTitle.scala")
        absent(generatedPostsDir / "split" / "PostTitleInput.scala")
        absent(generatedPostsDir / "split" / "Query.scala")
        absent(generatedPostsDir / "split" / "Subscription.scala")

        // From the 'potatoes' "split" config
        absent(generatedPotatoesDir / "package.scala")
        absent(generatedPotatoesDir / "Color.scala")
        absent(generatedPotatoesDir / "Mutation.scala")
        absent(generatedPotatoesDir / "Name.scala")
        absent(generatedPotatoesDir / "NameInput.scala")
        absent(generatedPotatoesDir / "Potato.scala")
        absent(generatedPotatoesDir / "Query.scala")
        absent(generatedPotatoesDir / "Subscription.scala")

        val postsTarget = (posts / target).value
        val potatoesTarget = (potatoes / target).value

        // Metadata files that will be generated and kept
        absent(postsTarget / "ctCalibanServer")
        absent(postsTarget / "ctCalibanServer" / "metadata")
        absent(potatoesTarget / "ctCalibanServer")
        absent(potatoesTarget / "ctCalibanServer" / "metadata")

        // Metadata files that will be generated and deleted
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_0.scala")
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_1.scala")
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_2.scala")
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_3.scala")
        absent(potatoesTarget / "ctCalibanServer" / "CalibanClientGenerator_0.scala")
      },
      TaskKey[Unit]("check-generated-files-post-compile") := {
        val generatedPostsDir = (postsClients / sourceManaged).value / "main" / "poc" / "caliban" / "client" / "generated" / "posts"
        val generatedPotatoesDir = (potatoesClients / Compile / sourceDirectory).value / "scala" / "poc" / "caliban" / "client" / "generated" / "potatoes"

        // From the 'posts' "default" config
        exists((postsClients / sourceManaged).value / "main" / "generated" / "Client.scala")

        // From the 'posts' "CalibanClient" config
        exists(generatedPostsDir / "CalibanClient.scala")

        // From the 'posts' "split" config
        exists(generatedPostsDir / "split" / "package.scala")
        exists(generatedPostsDir / "split" / "AuthorName.scala")
        exists(generatedPostsDir / "split" / "AuthorNameInput.scala")
        exists(generatedPostsDir / "split" / "Mutation.scala")
        exists(generatedPostsDir / "split" / "Post.scala")
        exists(generatedPostsDir / "split" / "PostContent.scala")
        exists(generatedPostsDir / "split" / "PostContentInput.scala")
        exists(generatedPostsDir / "split" / "PostId.scala")
        exists(generatedPostsDir / "split" / "PostTitle.scala")
        exists(generatedPostsDir / "split" / "PostTitleInput.scala")
        exists(generatedPostsDir / "split" / "Query.scala")
        exists(generatedPostsDir / "split" / "Subscription.scala")

        // From the 'potatoes' "split" config
        exists(generatedPotatoesDir / "package.scala")
        exists(generatedPotatoesDir / "Color.scala")
        exists(generatedPotatoesDir / "Mutation.scala")
        exists(generatedPotatoesDir / "Name.scala")
        exists(generatedPotatoesDir / "NameInput.scala")
        exists(generatedPotatoesDir / "Potato.scala")
        exists(generatedPotatoesDir / "Query.scala")
        exists(generatedPotatoesDir / "Subscription.scala")

        val postsTarget = (posts / target).value
        val potatoesTarget = (potatoes / target).value

        // "touch" files that were created and kept
        exists(postsTarget / "ctCalibanServer")
        exists(postsTarget / "ctCalibanServer" / "touch")
        exists(potatoesTarget / "ctCalibanServer")
        exists(potatoesTarget / "ctCalibanServer" / "touch")

        // Metadata files that were generated and deleted
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_0.scala")
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_1.scala")
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_2.scala")
        absent(postsTarget / "ctCalibanServer" / "CalibanClientGenerator_3.scala")
        absent(potatoesTarget / "ctCalibanServer" / "CalibanClientGenerator_0.scala")
      },
      TaskKey[Unit]("check-generated-potatoes-files-pre-move") := {
        val generatedDir = (potatoesClients / Compile / sourceDirectory).value / "scala" / "poc" / "caliban" / "client" / "generated" / "potatoes" / "moved"

        absent(generatedDir / "package.scala")
        absent(generatedDir / "Color.scala")
        absent(generatedDir / "Mutation.scala")
        absent(generatedDir / "Name.scala")
        absent(generatedDir / "NameInput.scala")
        absent(generatedDir / "Potato.scala")
        absent(generatedDir / "Query.scala")
        absent(generatedDir / "Subscription.scala")
      },
      TaskKey[Unit]("check-generated-potatoes-files-post-move") := {
        val generatedDir = (potatoesClients / Compile / sourceDirectory).value / "scala" / "poc" / "caliban" / "client" / "generated" / "potatoes" / "moved"

        exists(generatedDir / "package.scala")
        exists(generatedDir / "Color.scala")
        exists(generatedDir / "Mutation.scala")
        exists(generatedDir / "Name.scala")
        exists(generatedDir / "NameInput.scala")
        exists(generatedDir / "Potato.scala")
        exists(generatedDir / "Query.scala")
        exists(generatedDir / "Subscription.scala")
      }
    )

lazy val posts =
  project
    .in(file("modules/posts"))
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanServerSettings ++=
        Seq(
          "poc.caliban.posts.GraphQLApi.api" -> ClientGenerationSettings.default,
          "poc.caliban.posts.GraphQLApi.api" ->
            ClientGenerationSettings(
              packageName = "poc.caliban.client.generated.posts",
              clientName = "CalibanClient"
            ),
          "poc.caliban.posts.GraphQLApi.api" ->
            ClientGenerationSettings(
              packageName = "poc.caliban.client.generated.posts.split",
              splitFiles = true
            )
        )
    )
    .settings(libraryDependencies ++= calibanLib ++ zioTest)

lazy val potatoes =
  project
    .in(file("modules/potatoes"))
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanServerSettings :=
        Seq(
          "poc.caliban.potatoes.PotatoesApi.api" ->
            ClientGenerationSettings(
              packageName = "poc.caliban.client.generated.potatoes",
              splitFiles = true
            )
        )
    )
    .settings(libraryDependencies ++= calibanLib)

lazy val clients =
  project
    .in(file("modules/clients"))
    .settings(libraryDependencies ++= sttp)
    .dependsOn(postsClients, potatoesClients)

lazy val postsClients =
  project
    .withId("posts-clients")
    .in(file("modules/posts-clients"))
    .enablePlugins(CompileTimeCalibanClientPlugin)
    .settings(
      Compile / ctCalibanClient / ctCalibanClientsSettings      := Seq(posts),
      Compile / ctCalibanClient / ctCalibanClientsVersionedCode := false
    )

lazy val potatoesClients =
  project
    .withId("potatoes-clients")
    .in(file("modules/potatoes-clients"))
    .enablePlugins(CompileTimeCalibanClientPlugin)
    .settings(
      Compile / ctCalibanClient / ctCalibanClientsSettings := Seq(potatoes)
    )
