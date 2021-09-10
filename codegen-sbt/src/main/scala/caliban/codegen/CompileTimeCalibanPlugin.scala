package caliban.codegen

import caliban.codegen.CalibanSourceGenerator.TrackedSettings
import caliban.tools.compiletime.Utils
import sbt.Keys._
import sbt.io.IO.defaultCharset
import sbt.{ Compile, Def, Project, _ }

import java.io.File
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
              def generateGenerators: Seq[(File, (String, String))] =
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

                  val generatorFile = file(s"$metadataDir/$generatorName.scala")
                  sbt.IO.write(
                    file = generatorFile,
                    content = generatorCode,
                    charset = defaultCharset,
                    append = false
                  )

                  (
                    generatorFile,
                    (s"$generatorPackage.$generatorName", clientSettings.packageName)
                  )
                }

              val generateSources: Def.Initialize[Task[Seq[File]]] =
                Def.task(generateGenerators).map { generated =>
                  sbt.IO.writeLines(
                    file = file(s"$metadataDir/metadata"),
                    lines = generated.map { case (f, (a, b)) => s"${f.getAbsolutePath}#$a#$b" },
                    charset = defaultCharset,
                    append = false
                  )

                  generated.map(_._1)
                }

              /**
               * These settings are used to track the need to re-generate the code.
               *
               * When one of the value of these settings changes, then this plugin knows that it has to re-generate the code.
               */
              val trackedSettings: Def.Initialize[Task[TrackedSettings]] =
                Def.taskDyn {
                  Def.task {
                    TrackedSettings(
                      List(
                        caliban.codegen.BuildInfo.version,
                        zio.BuildInfo.version,
                        pluginSettings.mkString
                      )
                    )
                  }
                }

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
                      .flatTraverseT[File] { serverProject =>
                        Def.taskDyn {
                          val serverMetadata = {
                            val serverTargetDir = (serverProject / target).value.getAbsolutePath

                            waitForFile(() => file(s"$serverTargetDir/ctCalibanServer/metadata"))
                          }

                          val generatedRefs: Seq[(File, String, String)] =
                            sbt.IO.readLines(serverMetadata, defaultCharset).map { line =>
                              val Array(generatorFile, generatorRef, packageName) = line.split("#")
                              (file(generatorFile), generatorRef, packageName)
                            }

                          generatedRefs.flatTraverseT[File] { case (generatorFile, generatorRef, packageName) =>
                            Def.taskDyn {
                              def listGeneratedClientsFiles: Set[File] = {
                                val toPathDir: File = file(Utils.toPathDir(baseDirValue, packageName))

                                if (!toPathDir.exists()) {
                                  sbt.IO.createDirectory(toPathDir)
                                }

                                sbt.IO.listFiles(toPathDir).toSet
                              }

                              Def
                                .task[Set[File]](listGeneratedClientsFiles)
                                .flatMap { beforeGenDirFiles =>
                                  (serverProject / runMain)
                                    .toTask(s" $generatorRef $baseDirValue")
                                    .taskValue
                                    .map { _ =>
                                      sbt.IO.delete(generatorFile)

                                      val afterGenDirFiles: Set[File] = listGeneratedClientsFiles
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
              val trackedSettings: Def.Initialize[Task[TrackedSettings]] =
                Def.taskDyn {
                  import CompileTimeCalibanServerPlugin.autoImport._

                  // We need to track the server settings here so if they change the clients are re-generated.
                  val serverProjectSettings: Seq[(String, GenerateClientSettings)] =
                    Def.settingDyn {
                      clientsSettings.flatTraverseS(_ / ctCalibanServer / ctCalibanServerSettings)
                    }.value

                  Def.task {
                    TrackedSettings(
                      List(
                        caliban.codegen.BuildInfo.version,
                        zio.BuildInfo.version,
                        clientsSettings.map(_.id).mkString,
                        serverProjectSettings.mkString
                      )
                    )
                  }
                }

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

  @tailrec
  def waitForFile(file: () => File): File = {
    val f = file()

    if (f.exists()) f
    else {
      Thread.sleep(10)
      waitForFile(file)
    }
  }

  implicit final class SeqTaskOps[A](private val seq: Seq[A]) extends AnyVal {
    import sbt.Scoped.richTaskSeq

    def flatTraverseT[B](f: A => Def.Initialize[Task[Seq[B]]]): Def.Initialize[Task[Seq[B]]] =
      seq.map(f).join.map(_.flatten)

    def flatTraverseS[B](f: A => SettingKey[Seq[B]]): Def.Initialize[Seq[B]] =
      seq.map(f).join(_.flatten)
  }

  def cached(cacheName: String, trackedSettings: Def.Initialize[Task[TrackedSettings]])(
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

      cachedGenerateSources(trackedSettings.value) { () =>
        FilesInfo.exists((sourceManagedValue ** "*.scala").get.toSet).asInstanceOf[FilesInfo[PlainFileInfo]]
      }
    }
}
