package caliban.codegen

import caliban.codegen.CalibanSourceGenerator.TrackedSettings
import caliban.codegen.CompileTimeCalibanClientPlugin.autoImport.ctCaliban
import caliban.tools.CalibanCommonSettings
import sbt.Keys._
import sbt.{ Compile, Def, Project, _ }

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object CompileTimeCalibanServerPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = noTrigger

  object autoImport {

    /**
     * Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCalibanServer: TaskKey[Unit] = taskKey[Unit]("Plugin configuration keys namespace")

    // TODO: Should be a `SettingKey[Seq[String]]`? 🤔
    lazy val ctCalibanFullQualifiedCalibanApiRef: SettingKey[String] = settingKey[String]("TODO Jules")

    lazy val ctCalibanServerGenerate: TaskKey[Seq[File]] = taskKey[Seq[File]]("TODO Jules")
  }
  import autoImport._

  private val helpMsg: String =
    """
      |TODO: TO WRITE JULES
      |""".stripMargin

  private lazy val pluginSettings =
    inTask(ctCalibanServer)(
      Seq(
        ctCalibanFullQualifiedCalibanApiRef := "",
        ctCalibanServerGenerate :=
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log = streams.value.log("ctCalibanServer")

            val apiRef = (ctCalibanServer / ctCalibanFullQualifiedCalibanApiRef).value
            if (apiRef.isEmpty) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              val generatorCode =
                s"""
                   |package caliban.generator
                   |
                   |import caliban.tools.compiletime.CompileTime
                   |import zio.{ExitCode, URIO}
                   |
                   |private[generator] object CalibanClientGenerator extends zio.App {
                   |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
                   |    CompileTime.generateClient(args)($apiRef)
                   |  }
                   |}
                   |""".stripMargin

              // The location of this file is copied on `sbt-buildInfo` `BuildInfo` generated code.
              val generatorFile =
                new File(
                  s"${(thisProject / sourceManaged).value.absolutePath}/main/caliban-codegen-sbt/CalibanClientGenerator.scala"
                )
              log.warn(s"ctCalibanServer - generatorFile: ${generatorFile.absolutePath}")

              caliban.tools.compiletime.Utils.createDirectories(generatorFile.getParent)
              Files.writeString(generatorFile.toPath, generatorCode, StandardCharsets.UTF_8)

              Def.task(Seq(generatorFile))
            }
          }.value
      )
    )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban-tools" % BuildInfo.version % Compile
      ),
      (Compile / sourceGenerators) += Compile / ctCalibanServer / ctCalibanServerGenerate,
      (Test / sourceGenerators) += Test / ctCalibanServer / ctCalibanServerGenerate
    ) ++ inConfig(Compile)(pluginSettings) ++ inConfig(Test)(pluginSettings)
}

/**
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

    final case class GenerateClientsSettings(
      clientName: String,
      packageName: String,
      scalafmtPath: Option[String] = None,
      headers: List[(String, String)] = List.empty,
      genView: Boolean = false,
      scalarMappings: List[(String, String)] = List.empty,
      imports: List[String] = List.empty,
      splitFiles: Boolean = false,
      enableFmt: Boolean = true,
      extensibleEnums: Boolean = false
    )                              {
      private[caliban] def toCalibanCommonSettings: CalibanCommonSettings =
        CalibanCommonSettings(
          clientName = Some(clientName),
          scalafmtPath = scalafmtPath,
          headers = headers,
          packageName = Some(packageName),
          genView = Some(genView),
          scalarMappings = scalarMappings,
          imports = imports,
          splitFiles = Some(splitFiles),
          enableFmt = Some(enableFmt),
          extensibleEnums = Some(extensibleEnums)
        )
    }
    object GenerateClientsSettings {
      def default: GenerateClientsSettings = GenerateClientsSettings(clientName = "Client", packageName = "generated")
    }

    /**
     * Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCaliban: TaskKey[Unit] = taskKey[Unit]("Plugin configuration keys namespace")

    // Required Plugin configurations
    // TODO Jules: Should be a `SettingKey[(Project, GenerateClientsSettings)]`? 🤔
    lazy val ctCalibanSettings: SettingKey[(Project, GenerateClientsSettings)] =
      settingKey[(Project, GenerateClientsSettings)]("TODO Jules")

    // Plugin task
    lazy val ctCalibanGenerate: TaskKey[Seq[File]] = taskKey[Seq[File]](
      "Generate Caliban Client code at compile time. Automatically configured to be triggered when compilation is done."
    )

  }
  import autoImport._

  private val hiddenProjectId                                                   = "ctCaliban-hidden-default-project"
  private val emptySettings: Def.Initialize[(Project, GenerateClientsSettings)] =
    Def.settingDyn {
      Def.setting {
        (
          Project(
            id = hiddenProjectId,
            base = (thisProject / target).value
          ),
          GenerateClientsSettings.default
        )
      }
    }

  private lazy val pluginSettings =
    inTask(ctCaliban)(
      Seq(
        ctCalibanSettings := emptySettings.value,
        ctCalibanGenerate := {
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log = streams.value.log("ctCaliban")

            val (serverProject, _) = (ctCaliban / ctCalibanSettings).value
            if (serverProject.id == hiddenProjectId) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              val baseDirValue: String = (thisProject / baseDirectory).value.absolutePath
              log.debug(s"ctCaliban - baseDirValue: $baseDirValue")

              def generateSources: Def.Initialize[Task[Seq[File]]] =
                Def.taskDyn {
                  log.info(s"ctCaliban - Starting to generate...")

                  (serverProject / runMain)
                    .toTask(s" caliban.generator.CalibanClientGenerator $baseDirValue abc")
                    .map(_ => Seq.empty[File])
                }

              //log.info(s"ctCaliban - Generation done! 🎉")
              //val metadataFile  = file(s"$baseDirValue/target/ctCaliban/metadata")
              //val generatedFile = file(Files.readString(metadataFile.toPath, StandardCharsets.UTF_8))
              //Seq(generatedFile)

              /**
               * These settings are used to track the need to re-generate the code.
               *
               * When one of the value of these settings changes, then this plugin knows that it has to re-generate the code.
               */
              val cachedSettings: TrackedSettings =
                TrackedSettings(
                  List(
                    caliban.codegen.BuildInfo.version,
                    zio.BuildInfo.version
                    // TODO Jules: TO improve
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

  private def flatSequence[A](tasks: Seq[Def.Initialize[Task[Seq[A]]]]): Def.Initialize[Task[Seq[A]]] = {
    import sbt.Scoped.richTaskSeq

    tasks.join.map(_.flatten)
  }

}
