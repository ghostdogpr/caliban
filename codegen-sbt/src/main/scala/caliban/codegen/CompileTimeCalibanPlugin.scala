package caliban.codegen

import sbt.Keys._
import sbt.{ Compile, Def, _ }

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
    s"""
       |Missing configuration for the "CompileTimeCalibanPlugin" plugin.
       |
       |You need to configure the reference of the class containing the call to `CompileTime.generateClient`:
       |
       |```
       |Compile / ctCaliban / ctCalibanGeneratorAppRef := Some("com.example.GeneratorApp")
       |```
       |
       |See documentation for more details: (TODO Add link)
       |
       |""".stripMargin

  object autoImport {

    /**
     * Tasks and settings namespace
     *
     * (https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html#Using+a+%E2%80%9Cmain%E2%80%9D+task+scope+for+settings)
     */
    lazy val ctCaliban = taskKey[Unit]("Plugin configuration keys namespace")

    // Required Plugin configurations
    lazy val ctCalibanServerProject = settingKey[Option[Project]](
      "(Required) sbt project where the CompileTimeCalibanServerPlugin is enabled"
    )

    // Optional Plugin configurations
    lazy val ctCalibanGeneratorAppRef = settingKey[String](
      "(Optional) Reference of the zio.App class containing the call to `CompileTime.generateClient(/* your API */)`. Default: `generator.CalibanClientGenerator`"
    )
    lazy val ctCalibanClientName      = settingKey[String](
      "(Optional) Generated client name. Default: `Client`"
    )
    lazy val ctCalibanPackageName     = settingKey[String](
      "(Optional) Generated client package. Default: `generated`"
    )

    // Plugin task
    lazy val ctCalibanGenerate = taskKey[Seq[File]](
      "Generate Caliban Client code at compile time. Automatically configured to be triggered when compilation is done."
    )
  }
  import autoImport._

  private lazy val ctCalibanSettings =
    inTask(ctCaliban)(
      Seq(
        ctCalibanServerProject := None,
        ctCalibanGeneratorAppRef := "generator.CalibanClientGenerator",
        ctCalibanClientName := "Client",
        ctCalibanPackageName := "generated",
        ctCalibanGenerate :=
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def
            .taskDyn[Seq[File]] {
              val log                     = streams.value.log("ctCaliban")
              val maybeServerProjectValue = (ctCaliban / ctCalibanServerProject).value

              log.debug(s"ctCaliban - serverProject: $maybeServerProjectValue")

              maybeServerProjectValue match {
                case None                     => Def.task { log.error(helpMsg); List.empty[File] }
                case Some(serverProjectValue) =>
                  val baseDirValue         = baseDirectory.value
                  val clientNameValue      = (ctCaliban / ctCalibanClientName).value
                  val packageNameValue     = (ctCaliban / ctCalibanPackageName).value
                  val toPath               = s"$baseDirValue/src/main/scala/${packagePath(packageNameValue)}/$clientNameValue.scala"
                  val generatorAppRefValue = (ctCaliban / ctCalibanGeneratorAppRef).value

                  log.debug(s"ctCaliban - baseDirectory: $baseDirValue")
                  log.debug(s"ctCaliban - clientName: $clientNameValue")
                  log.debug(s"ctCaliban - packageName: $packageNameValue")
                  log.debug(s"ctCaliban - toPath: $toPath")
                  log.debug(s"ctCaliban - generatorAppRef: $generatorAppRefValue")

                  Def.task[Seq[File]] {
                    log.info(s"ctCaliban - Starting to generate...")

                    val res = Def.task {
                      val res = file(toPath)
                      sbt.IO.createDirectory(res.getParentFile)
                      res
                    }.value

                    (serverProjectValue / runMain)
                      .toTask(s" $generatorAppRefValue $toPath $packageNameValue $clientNameValue")
                      .value

                    log.info(s"ctCaliban - Generation done! 🎉")

                    List(res)
                  }
              }

            }
            .value
      )
    )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban-client" % BuildInfo.version
      ),
      (Compile / sourceGenerators) += Compile / ctCaliban / ctCalibanGenerate,
      (Test / sourceGenerators) += Test / ctCaliban / ctCalibanGenerate
    ) ++ inConfig(Compile)(ctCalibanSettings) ++ inConfig(Test)(ctCalibanSettings)

  private def packagePath(packageName: String): String = packageName.replaceAll("\\.", "/")

}
