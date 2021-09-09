package caliban.codegen

import caliban.codegen.CalibanSourceGenerator.TrackedSettings
import caliban.tools.compiletime.Utils
import sbt.Keys._
import sbt.{ Compile, Def, Project, _ }
import zio.prelude._

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.annotation.tailrec

object CompileTimeCalibanServerPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = noTrigger

  object autoImport {

    /* ## Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCalibanServer: TaskKey[Unit] = taskKey[Unit]("Plugin configuration keys namespace")

    // ## Required Plugin configurations
    lazy val ctCalibanServerApiRefs: SettingKey[Seq[String]] = settingKey[Seq[String]]("TODO Jules")

    // ## Plugin task
    lazy val ctCalibanServerGenerate: TaskKey[Seq[File]] = taskKey[Seq[File]]("Internal task")
  }
  import autoImport._

  private val helpMsg: String =
    """
      |TODO: TO WRITE JULES
      |""".stripMargin

  private lazy val pluginSettings =
    inTask(ctCalibanServer)(
      Seq(
        ctCalibanServerApiRefs := Seq.empty,
        ctCalibanServerGenerate :=
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log = streams.value.log("ctCalibanServer")

            val apiRefs: Seq[String] = (ctCalibanServer / ctCalibanServerApiRefs).value
            if (apiRefs.isEmpty) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              def generateGenerators: Seq[(File, String)] =
                apiRefs.zipWithIndex.flatMap { case (ref, i) =>
                  val packageName   = "caliban.generator"
                  val name          = s"CalibanClientGenerator_$i"
                  val generatorCode =
                    s"""
                       |package $packageName
                       |
                       |import caliban.tools.compiletime.CompileTime
                       |import zio.{ExitCode, URIO}
                       |
                       |private[generator] object $name extends zio.App {
                       |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
                       |    CompileTime.generateClient(args)($ref)
                       |  }
                       |}
                       |""".stripMargin

                  // The location of this file is copied on `sbt-buildInfo` `BuildInfo` generated code.
                  val generatorFile =
                    new File(
                      s"${(thisProject / sourceManaged).value.absolutePath}/main/caliban-codegen-sbt/$name.scala"
                    )

                  Utils.createDirectories(generatorFile.getParent)
                  Files.writeString(generatorFile.toPath, generatorCode, StandardCharsets.UTF_8)

                  Seq(generatorFile -> s"$packageName.$name")
                }

              Def.task(generateGenerators).map { tmp =>
                val (generatedFiles, generatedRefs) = tmp.unzip

                val metaDir = s"${(thisProject / target).value.getAbsolutePath}/ctCalibanServer"
                Utils.createDirectories(metaDir)
                Files.writeString(
                  new File(s"$metaDir/metadata").toPath,
                  generatedRefs.mkString("\n"),
                  StandardCharsets.UTF_8
                )

                generatedFiles
              }
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
      |You need to configure the `Compile / ctCalibanClient / ctCalibanServerProject` setting to point to your "server" module:
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
      |    .settings(Compile / ctCalibanClient / ctCalibanServerProject := server)
      |```
      |
      |See documentation for more details: (TODO Add link)
      |
      |""".stripMargin.trim // TODO Jules: To Rewrite

  object autoImport extends caliban.tools.compiletime.Config {

    /* ## Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCalibanClient: TaskKey[Unit] = taskKey[Unit]("Plugin configuration keys namespace")

    // ## Required Plugin configurations
    lazy val ctCalibanClientsSettings: SettingKey[Map[Project, Seq[GenerateClientSettings]]] =
      settingKey[Map[Project, Seq[GenerateClientSettings]]]("TODO Jules")

    // ## Plugin task
    lazy val ctCalibanClientGenerate: TaskKey[Seq[File]] = taskKey[Seq[File]](
      "Generate Caliban Client(s) code at compile time. Automatically configured to be triggered when compilation is done."
    )

    /**
     * To understand why we need this weird function, see: https://stackoverflow.com/a/16466541/2431728
     */
    private[caliban] def toCalibanTools(
      s: GenerateClientSettings
    ): caliban.tools.compiletime.Config.GenerateClientSettings =
      caliban.tools.compiletime.Config.GenerateClientSettings(
        clientName = s.clientName,
        packageName = s.packageName,
        scalafmtPath = s.scalafmtPath,
        headers = s.headers,
        genView = s.genView,
        scalarMappings = s.scalarMappings,
        imports = s.imports,
        splitFiles = s.splitFiles,
        enableFmt = s.enableFmt,
        extensibleEnums = s.extensibleEnums
      )
  }
  import autoImport._

  private object GeneratedRef extends Subtype[String]
  private type GeneratedRef = GeneratedRef.Type

  /**
   * I have to apologize for the readability and complexity of this code.
   * I did my best with the limited knowledge I have and the constraints that sbt is putting on us 😕
   */
  private lazy val pluginSettings =
    inTask(ctCalibanClient)(
      Seq(
        ctCalibanClientsSettings := Map.empty,
        ctCalibanClientGenerate := {
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log = streams.value.log("ctCalibanClient")

            val clientsSettings = (ctCalibanClient / ctCalibanClientsSettings).value
            if (clientsSettings.isEmpty) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              val baseDirValue: String = (thisProject / baseDirectory).value.absolutePath

              def generateSources: Def.Initialize[Task[Seq[File]]] = {
                import Functions._

                log.info(s"ctCalibanClient - Starting to generate...")

                clientsSettings.toSeq
                  .flatTraverse[File] { case (serverProject, clientSettings) =>
                    Def.taskDyn {
                      def serverMetadata =
                        new File(
                          s"${(serverProject / target).value.getAbsolutePath}/ctCalibanServer/metadata"
                        )

                      @tailrec
                      def waitForFile(): Unit =
                        if (serverMetadata.exists()) ()
                        else {
                          Thread.sleep(100)
                          waitForFile()
                        }

                      waitForFile()

                      val generatedRefs: Seq[GeneratedRef] =
                        Files
                          .readString(serverMetadata.toPath)
                          .mkString
                          .split('\n')
                          .map(GeneratedRef.wrap)

                      clientSettings.zip(generatedRefs).flatTraverse[File] { case (clientSettings, generatedRef) =>
                        Def.taskDyn {
                          val toPathDir: File =
                            new File(Utils.toPath(baseDirValue, toCalibanTools(clientSettings))).getParentFile

                          Def
                            .task[Set[File]] {
                              // If I don't put the following code in a Task, it's not executed. IDK why. 🤷
                              Utils.createDirectories(toPathDir.absolutePath)
                              toPathDir.listFiles().toSet
                            }
                            .flatMap { beforeGenDirFiles =>
                              (serverProject / runMain)
                                .toTask(s" $generatedRef $baseDirValue ${clientSettings.asArgs.mkString(" ")}")
                                .taskValue
                                .map { _ =>
                                  val afterGenDirFiles: Set[File] = toPathDir.listFiles().toSet
                                  (afterGenDirFiles diff beforeGenDirFiles).toSeq
                                }
                            }
                        }
                      }
                    }
                  }
                  .map { result: Seq[File] =>
                    log.info(s"ctCalibanClient - Generation done! 🎉")
                    result
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
                    clientsSettings.map { case (project, settings) => project.id -> settings }.mkString
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

                Tracked.inputChanged(cacheDirectory / "ctCalibanClient-inputs") {
                  (inChanged: Boolean, _: TrackedSettings) =>
                    Tracked.outputChanged(cacheDirectory / "ctCalibanClient-output") {
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
      (Compile / sourceGenerators) += Compile / ctCalibanClient / ctCalibanClientGenerate,
      (Test / sourceGenerators) += Test / ctCalibanClient / ctCalibanClientGenerate
    ) ++ inConfig(Compile)(pluginSettings) ++ inConfig(Test)(pluginSettings)

}

private[caliban] object Functions {
  import sbt.Scoped.richTaskSeq

  def flatSequence[A](tasks: Seq[Def.Initialize[Task[Seq[A]]]]): Def.Initialize[Task[Seq[A]]] =
    tasks.join.map(_.flatten)

  implicit final class SeqTaskOps[A](private val seq: Seq[A]) extends AnyVal {
    def flatTraverse[B](f: A => Def.Initialize[Task[Seq[B]]]): Def.Initialize[Task[Seq[B]]] = flatSequence(seq.map(f))
  }
}
