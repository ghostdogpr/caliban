package caliban.codegen

import caliban.codegen.CalibanSourceGenerator.TrackedSettings
import caliban.tools.CalibanCommonSettings
import caliban.tools.compiletime.CompileTimeUtils
import sbt.Keys._
import sbt.{ Compile, Def, _ }

import java.io.File

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
        "com.github.ghostdogpr" %% "caliban-tools" % BuildInfo.version % Compile,
        "dev.zio"               %% "zio"           % "1.0.11"          % Compile
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
    lazy val ctCalibanSettings        = settingKey[CalibanCommonSettings]("""
                                                                     |(Optional) Settings for the generator.
                                                                     |
                                                                     |Default values used:
                                                                     | - clientName: "Client"
                                                                     | - packageName: "generated"
                                                                     |""".stripMargin)

    // Plugin task
    lazy val ctCalibanGenerate = taskKey[Seq[File]](
      "Generate Caliban Client code at compile time. Automatically configured to be triggered when compilation is done."
    )

    def ctCalibanSettingsBuilder(setting: CalibanCommonSettings => CalibanCommonSettings): CalibanCommonSettings =
      setting.apply(CalibanCommonSettings.empty)
  }
  import autoImport._

  private val hiddenProjectId = "ctCaliban-hidden-default-project"

  private lazy val pluginSettings =
    inTask(ctCaliban)(
      Seq(
        ctCalibanServerProject := Project(id = hiddenProjectId, base = (thisProject / target).value),
        ctCalibanGeneratorAppRef := "generator.CalibanClientGenerator",
        ctCalibanSettings := CalibanCommonSettings.empty,
        ctCalibanGenerate := {
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log = streams.value.log("ctCaliban")

            val serverProject = (ctCaliban / ctCalibanServerProject).value
            if (serverProject.id == hiddenProjectId) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              def generateSources: Def.Initialize[Task[Seq[File]]] =
                Def.taskDyn {
                  val baseDirValue: File              = (thisProject / baseDirectory).value
                  val settings: CalibanCommonSettings = (ctCaliban / ctCalibanSettings).value
                  val clientNameValue: String         = settings.clientName.getOrElse("Client")
                  val packageNameValue: String        = settings.packageName.getOrElse("generated")
                  val toPath: String                  =
                    s"$baseDirValue/src/main/scala/${packagePath(packageNameValue)}/$clientNameValue.scala"

                  val generatorAppRefValue: String = (ctCaliban / ctCalibanGeneratorAppRef).value

                  log.debug(s"ctCaliban - baseDirectory: $baseDirValue")
                  log.debug(s"ctCaliban - settings: $settings")
                  log.debug(s"ctCaliban - clientName: $clientNameValue")
                  log.debug(s"ctCaliban - packageName: $packageNameValue")
                  log.debug(s"ctCaliban - toPath: $toPath")
                  log.debug(s"ctCaliban - generatorAppRef: $generatorAppRefValue")

                  Def.task {
                    log.info(s"ctCaliban - Starting to generate...")

                    Def.task {
                      sbt.IO.createDirectory(file(toPath).getParentFile)
                    }.value

                    (serverProject / runMain).toTask {
                      val calibanSettings: List[String] =
                        CompileTimeUtils.calibanCommonSettingsEquivalence
                          .to(settings.copy(clientName = Some(clientNameValue), packageName = Some(packageNameValue)))

                      s" $generatorAppRefValue $toPath ${calibanSettings.mkString(" ")}"
                    }.value

                    log.info(s"ctCaliban - Generation done! 🎉")
                    Seq(file(toPath))
                  }
                }

              /**
               * These settings are used to track the need to re-generate the code.
               *
               * When one of the value of these settings changes, then this plugin knows that it has to re-generate the code.
               *
               * \@guizmaii's note:
               * ----------------
               * One nice thing to have would be to cache the content of the `ctCalibanGeneratorAppRef` file so when the user changes
               * its content, this plugin knows that it has to re-generate the code.
               * I tried to implement it, failed and abandoned. Working with sbt is too time consuming.
               * GLHF if you want to give it a try.
               */
              val cachedSettings: TrackedSettings =
                TrackedSettings(
                  List(
                    caliban.codegen.BuildInfo.version,
                    zio.BuildInfo.version,
                    serverProject.id,
                    (ctCaliban / ctCalibanGeneratorAppRef).value,
                    (ctCaliban / ctCalibanSettings).value.toString
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

  private def packagePath(packageName: String): String = packageName.replaceAll("\\.", "/")

}
