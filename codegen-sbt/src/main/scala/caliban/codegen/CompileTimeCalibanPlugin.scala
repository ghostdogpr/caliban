package caliban.codegen

import sbt.Keys._
import sbt.librarymanagement.Resolver
import sbt.{ Compile, Def, _ }

/**
 * Interesting documentations about writing sbt plugins:
 *  - https://www.scala-sbt.org/1.x/docs/Plugins.html
 *  - https://www.scala-sbt.org/1.x/docs/Plugins-Best-Practices.html
 */
object CompileTimeCalibanPlugin extends AutoPlugin {
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

    // Plugin configuration
    lazy val ctCalibanGeneratorAppRef = settingKey[Option[String]](
      "(Required) Reference of the zio.App class containing the call to `CompileTime.generateClient(/* your API */)`"
    )
    lazy val ctCalibanClientName      = settingKey[String](
      "(Optional) Generated client name. Default: `Client`"
    )
    lazy val ctCalibanPackageName     = settingKey[String](
      "(Optional) Generated client package. Default: `generated`"
    )

    // Plugin task
    lazy val ctCalibanGenerate = taskKey[Unit](
      "Generate Caliban Client code at compile time. Automatically configured to be triggered when compilation is done."
    )
  }
  import autoImport._

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      resolvers += Resolver.mavenLocal, // TODO Jules: TO DELETE
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban-client" % BuildInfo.version,
        "com.github.ghostdogpr" %% "caliban-tools"  % BuildInfo.version % Compile,
        "dev.zio"               %% "zio"            % "1.0.11"          % Compile
      )
    ) ++ inTask(ctCaliban)(
      Seq(
        ctCalibanGeneratorAppRef := None,
        ctCalibanClientName := "Client",
        ctCalibanPackageName := "generated",
        ctCalibanGenerate :=
          // That helped: https://stackoverflow.com/q/26244115/2431728
          Def.taskDyn {
            val log                       = streams.value.log("ctCaliban")
            val maybeGeneratorAppRefValue = (ctCaliban / ctCalibanGeneratorAppRef).value
            log.debug(s"ctCaliban - generatorAppRef: $maybeGeneratorAppRefValue")

            maybeGeneratorAppRefValue match {
              case None                       => Def.task(log.error(helpMsg))
              case Some(generatorAppRefValue) =>
                val baseDirValue     = baseDirectory.value
                val clientNameValue  = (ctCaliban / ctCalibanClientName).value
                val packageNameValue = (ctCaliban / ctCalibanPackageName).value
                val toPath           = s"$baseDirValue/src/main/scala/${packagePath(packageNameValue)}/$clientNameValue.scala"

                Def.task {
                  log.debug(s"ctCaliban - baseDirectory: $baseDirValue")
                  log.debug(s"ctCaliban - clientName: $clientNameValue")
                  log.debug(s"ctCaliban - packageName: $packageNameValue")
                  log.debug(s"ctCaliban - toPath: $toPath")

                  log.info(s"ctCaliban - Starting to generate...")

                  Def.task {
                    sbt.IO.createDirectory(file(toPath).getParentFile)
                  }.value

                  (Compile / thisProject / runMain)
                    .toTask(s" $generatorAppRefValue $toPath $packageNameValue $clientNameValue")
                    .value

                  log.info(s"ctCaliban - Generation done! 🎉")
                }
            }
          }.triggeredBy(Compile / compile).value
      )
    )

  private def packagePath(packageName: String): String = packageName.replaceAll("\\.", "/")

}
