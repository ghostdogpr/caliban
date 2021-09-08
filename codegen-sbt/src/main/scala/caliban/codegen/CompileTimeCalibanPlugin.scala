package caliban.codegen

import caliban.codegen.CalibanSourceGenerator.TrackedSettings
import sbt.Keys._
import sbt.{ Compile, Def, _ }

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/**
 * User-oriented documentation:
 * ----------------------------
 *
 * TODO Jules: Write doc
 */
object CompileTimeCalibanServerPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = noTrigger

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban-tools" % BuildInfo.version % Compile
      )
    )
}

/**
 * User-oriented documentation:
 * ----------------------------
 *
 * TODO Jules: Write doc
 *
 * Sbt plugin authors documentation:
 * ---------------------------------
 *
 * Interesting documentations about how to write sbt plugins:
 *  - https://www.scala-sbt.org/1.x/docs/Plugins.html
 *  - https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html
 */
object CompileTimeCalibanClientPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = noTrigger

  private def helpMsg: String =
    """
      |Missing configuration for the "CompileTimeCalibanClientPlugin" plugin.
      |
      |You need to configure the `Compile / ctCaliban / ctCalibanServerProject` setting to point to your "server" module:
      |
      |```
      |lazy val server =
      |  project
      |    ...
      |    .enablePlugins(CompileTimeCalibanServerPlugin)
      |    ...
      |
      |lazy val calibanClient =
      |  project
      |    ...
      |    .enablePlugins(CompileTimeCalibanClientPlugin)
      |    ...
      |    .settings(Compile / ctCaliban / ctCalibanServerProject := server)
      |```
      |
      |See documentation for more details: (TODO Add link)
      |
      |""".stripMargin.trim

  object autoImport {

    /**
     * Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCaliban = taskKey[Unit]("Plugin configuration keys namespace")

    // Required Plugin configurations
    lazy val ctCalibanServerProject = settingKey[Project](
      "(Required) sbt project where the CompileTimeCalibanServerPlugin is enabled"
    )

    // Optional Plugin configurations
    lazy val ctCalibanGeneratorAppRef = settingKey[String](
      "(Optional) Reference of the zio.App class containing the call to `CompileTime.generateClient(/* your API */)`. Default: `generator.CalibanClientGenerator`"
    )

    // Plugin task
    lazy val ctCalibanGenerate = taskKey[Seq[File]](
      "Generate Caliban Client code at compile time. Automatically configured to be triggered when compilation is done."
    )

  }
  import autoImport._

  private val hiddenProjectId = "ctCaliban-hidden-default-project"

  private lazy val pluginSettings =
    inTask(ctCaliban)(
      Seq(
        ctCalibanServerProject := Project(id = hiddenProjectId, base = (thisProject / target).value),
        ctCalibanGeneratorAppRef := "generator.CalibanClientGenerator",
        ctCalibanGenerate := {
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log = streams.value.log("ctCaliban")

            val serverProject = (ctCaliban / ctCalibanServerProject).value
            if (serverProject.id == hiddenProjectId) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              def generateSources: Def.Initialize[Task[Seq[File]]] =
                Def.taskDyn {
                  val baseDirValue: String         = (thisProject / baseDirectory).value.absolutePath
                  val generatorAppRefValue: String = (ctCaliban / ctCalibanGeneratorAppRef).value

                  log.debug(s"ctCaliban - baseDirValue: $baseDirValue")
                  log.debug(s"ctCaliban - generatorAppRef: $generatorAppRefValue")

                  Def.task {
                    log.info(s"ctCaliban - Starting to generate...")

                    (serverProject / runMain)
                      .toTask(s" $generatorAppRefValue $baseDirValue")
                      .value

                    log.info(s"ctCaliban - Generation done! 🎉")
                    val metadataFile  = file(s"$baseDirValue/target/ctCaliban/metadata")
                    val generatedFile = file(Files.readString(metadataFile.toPath, StandardCharsets.UTF_8))
                    Seq(generatedFile)
                  }
                }

              /**
               * These settings are used to track the need to re-generate the code.
               *
               * When one of the value of these settings changes, then this plugin knows that it has to re-generate the code.
               */
              val cachedSettings: TrackedSettings =
                TrackedSettings(
                  List(
                    caliban.codegen.BuildInfo.version,
                    zio.BuildInfo.version,
                    serverProject.id,
                    (ctCaliban / ctCalibanGeneratorAppRef).value
                  )
                )

              val cacheDirectory = streams.value.cacheDirectory

              /**
               * Copied and adapted from [[CalibanSourceGenerator]] cache mechanism,
               * which was itself, I quote, "heavily inspired by the caching technique from eed3si9n's sbt-scalaxb plugin".
               *
               * I wasn't able to add the source in the cache as it's done in [[CalibanSourceGenerator]] because it
               * creates a cyclic dependency. Not sure we need it anyway.
               */
              val cachedGenerateSources
                : TrackedSettings => (() => FilesInfo[PlainFileInfo]) => Def.Initialize[Task[Seq[File]]] = {
                import sbt.util.CacheImplicits._

                Tracked.inputChanged(cacheDirectory / "ctCaliban-inputs") { (inChanged: Boolean, _: TrackedSettings) =>
                  Tracked.outputChanged(cacheDirectory / "ctCaliban-output") {
                    (outChanged: Boolean, outputs: FilesInfo[PlainFileInfo]) =>
                      Def.taskIf {
                        if (inChanged || outChanged) generateSources.value
                        else outputs.files.toList.map(_.file)
                      }
                  }
                }
              }

              val sourceManagedValue: File = sourceManaged.value

              cachedGenerateSources(cachedSettings) { () =>
                FilesInfo.exists((sourceManagedValue ** "*.scala").get.toSet).asInstanceOf[FilesInfo[PlainFileInfo]]
              }
            }
          }.value
        }
      )
    )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban-client" % BuildInfo.version
      ),
      (Compile / sourceGenerators) += Compile / ctCaliban / ctCalibanGenerate,
      (Test / sourceGenerators) += Test / ctCaliban / ctCalibanGenerate
    ) ++ inConfig(Compile)(pluginSettings) ++ inConfig(Test)(pluginSettings)

}
