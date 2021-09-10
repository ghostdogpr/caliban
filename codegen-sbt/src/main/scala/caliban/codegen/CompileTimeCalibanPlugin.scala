package caliban.codegen

import caliban.codegen.CalibanSourceGenerator.TrackedSettings
import caliban.tools.compiletime.Utils
import sbt.Keys._
import sbt.{ Compile, Def, Project, _ }

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.annotation.tailrec

object CompileTimeCalibanServerPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = noTrigger

  object autoImport extends caliban.tools.compiletime.Config {

    /* ## Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCalibanServer: TaskKey[Unit] = taskKey[Unit]("Plugin configuration keys namespace")

    // ## Required Plugin configurations
    lazy val ctCalibanServerSettings: SettingKey[Seq[(String, GenerateClientSettings)]] =
      settingKey[Seq[(String, GenerateClientSettings)]]("TODO Jules")

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
        ctCalibanServerSettings := Seq.empty,
        ctCalibanServerGenerate :=
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            import Functions._

            val log         = streams.value.log("ctCalibanServer")
            val metadataDir = s"${(thisProject / target).value.getAbsolutePath}/ctCalibanServer"

            val pluginSettings: Seq[(String, GenerateClientSettings)] =
              (ctCalibanServer / ctCalibanServerSettings).value
            if (pluginSettings.isEmpty) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              def generateGenerators: Seq[(File, (String, String, String))] =
                pluginSettings.zipWithIndex.map { case ((ref, clientSettings), i) =>
                  val generatorPackage = "caliban.generator"
                  val generatorName    = s"CalibanClientGenerator_$i"
                  val generatorCode    =
                    s"""
                       |package $generatorPackage
                       |
                       |import caliban.tools.compiletime.CompileTime
                       |import caliban.tools.compiletime.Config.GenerateClientSettings
                       |import zio.{ExitCode, URIO}
                       |
                       |private[generator] object $generatorName extends zio.App {
                       |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
                       |    CompileTime.generateClient(args)(
                       |      $ref,
                       |      ${clientSettings.asScalaCode}
                       |    )
                       |}
                       |""".stripMargin.trim

                  val generatorFile: File = new File(s"$metadataDir/$generatorName.scala")
                  Utils.createDirectories(generatorFile.getParent)
                  Files.writeString(generatorFile.toPath, generatorCode, StandardCharsets.UTF_8)

                  (
                    generatorFile,
                    (s"$generatorPackage.$generatorName", clientSettings.packageName, clientSettings.clientName)
                  )
                }

              val generateSources: Def.Initialize[Task[Seq[File]]] =
                Def.task(generateGenerators).map { generated =>
                  Utils.createDirectories(metadataDir)
                  Files.writeString(
                    new File(s"$metadataDir/metadata").toPath,
                    generated.map { case (f, (a, b, c)) => s"${f.getAbsolutePath}#$a#$b#$c" }.mkString("\n"),
                    StandardCharsets.UTF_8
                  )

                  generated.map(_._1)
                }

              /**
               * These settings are used to track the need to re-generate the code.
               *
               * When one of the value of these settings changes, then this plugin knows that it has to re-generate the code.
               */
              val trackedSettings: TrackedSettings =
                TrackedSettings(
                  List(
                    caliban.codegen.BuildInfo.version,
                    zio.BuildInfo.version,
                    pluginSettings.mkString
                  )
                )

              cached("ctCalibanServer", trackedSettings)(generateSources)
            }
          }.value
      )
    )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies += "com.github.ghostdogpr" %% "caliban-tools" % BuildInfo.version % Compile,
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

  object autoImport {

    /* ## Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCalibanClient: TaskKey[Unit] = taskKey[Unit]("Plugin configuration keys namespace")

    // ## Required Plugin configurations
    lazy val ctCalibanClientsSettings: SettingKey[Seq[Project]] = settingKey[Seq[Project]]("TODO Jules")

    // ## Plugin task
    lazy val ctCalibanClientGenerate: TaskKey[Seq[File]] = taskKey[Seq[File]](
      "Generate Caliban Client(s) code at compile time. Automatically configured to be triggered when compilation is done."
    )
  }
  import autoImport._

  /**
   * I have to apologize for the readability and complexity of this code.
   * I did my best with the limited knowledge I have and the constraints that sbt is putting on us 😕
   */
  private lazy val pluginSettings =
    inTask(ctCalibanClient)(
      Seq(
        ctCalibanClientsSettings := Seq.empty,
        ctCalibanClientGenerate := {
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            import Functions._

            val log = streams.value.log("ctCalibanClient")

            val clientsSettings: Seq[Project] = (ctCalibanClient / ctCalibanClientsSettings).value
            if (clientsSettings.isEmpty) Def.task { log.error(helpMsg); Seq.empty[File] }
            else {
              val baseDirValue: String = (thisProject / baseDirectory).value.absolutePath

              def generateSources: Def.Initialize[Task[Seq[File]]] =
                Def.taskDyn {
                  log.info(s"ctCalibanClient - Starting to generate...")

                  Def.task {
                    clientsSettings
                      .flatTraverse[File] { serverProject =>
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

                          val generatedRefs: Seq[(File, String, String, String)] =
                            Files
                              .readString(serverMetadata.toPath)
                              .mkString
                              .split('\n')
                              .map { v =>
                                val Array(generatorFile, generatorRef, packageName, clientName) = v.split("#")
                                (new File(generatorFile), generatorRef, packageName, clientName)
                              }

                          generatedRefs.flatTraverse[File] {
                            case (generatorFile, generatorRef, packageName, clientName) =>
                              Def.taskDyn {
                                val toPathDir: File =
                                  new File(Utils.toPath(baseDirValue, packageName, clientName)).getParentFile

                                Def
                                  .task[Set[File]] {
                                    // If I don't put the following code in a Task, it's not executed. IDK why. 🤷
                                    Utils.createDirectories(toPathDir.getAbsolutePath)
                                    toPathDir.listFiles().toSet
                                  }
                                  .flatMap { beforeGenDirFiles =>
                                    (serverProject / runMain)
                                      .toTask(s" $generatorRef $baseDirValue")
                                      .taskValue
                                      .map { _ =>
                                        Files.deleteIfExists(generatorFile.toPath)

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
                      .value
                  }
                }

              /**
               * These settings are used to track the need to re-generate the code.
               *
               * When one of the value of these settings changes, then this plugin knows that it has to re-generate the code.
               */
              val trackedSettings: TrackedSettings =
                TrackedSettings(
                  List(
                    caliban.codegen.BuildInfo.version,
                    zio.BuildInfo.version,
                    clientsSettings.map(_.id).mkString
                  )
                )

              cached("ctCalibanClient", trackedSettings)(generateSources)
            }
          }.value
        }
      )
    )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies += "com.github.ghostdogpr" %% "caliban-client" % BuildInfo.version,
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

  def cached(cacheName: String, trackedSettings: TrackedSettings)(
    generateSources: Def.Initialize[Task[Seq[File]]]
  ): Def.Initialize[Task[Seq[File]]] =
    Def.taskDyn {
      val cacheDirectory = streams.value.cacheDirectory

      /**
       * Copied and adapted from [[CalibanSourceGenerator]] cache mechanism,
       * which was itself, I quote, "heavily inspired by the caching technique from eed3si9n's sbt-scalaxb plugin".
       */
      val cachedGenerateSources
        : TrackedSettings => (() => FilesInfo[PlainFileInfo]) => Def.Initialize[Task[Seq[File]]] = {
        import sbt.util.CacheImplicits._

        Tracked.inputChanged(cacheDirectory / s"$cacheName-inputs") { (inChanged: Boolean, _: TrackedSettings) =>
          Tracked.outputChanged(cacheDirectory / s"$cacheName-output") {
            (outChanged: Boolean, outputs: FilesInfo[PlainFileInfo]) =>
              Def.taskIf {
                if (inChanged || outChanged) generateSources.value
                else outputs.files.toList.map(_.file)
              }
          }
        }
      }

      val sourceManagedValue: File = sourceManaged.value

      cachedGenerateSources(trackedSettings) { () =>
        FilesInfo.exists((sourceManagedValue ** "*.scala").get.toSet).asInstanceOf[FilesInfo[PlainFileInfo]]
      }
    }
}
