package caliban.codegen

import caliban.GraphQL
import sbt.Keys._
import sbt._

import java.io.File
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object CalibanPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin

  object autoImport extends CalibanKeys
  import autoImport._

  lazy val baseSettings = Seq(
    caliban := (caliban / calibanGenerator).value,
    (caliban / sourceManaged) := {
      sourceManaged.value / "caliban-codegen-sbt"
    },
    (caliban / calibanSources) := {
      if (Seq(Compile, Test).contains(configuration.value)) sourceDirectory.value / "graphql"
      else sourceDirectory.value / "main" / "graphql"
    },
    caliban / calibanSettings := Seq.empty
  )

  lazy val calibanScopedSettings = inTask(caliban)(
    Seq(
      sources := (calibanSources.value ** "*.graphql").get.sorted,
      clean := {
        val sourceDir = sourceManaged.value
        IO.delete((sourceDir ** "*").get)
        IO.createDirectory(sourceDir)
      },
      calibanGenerator := {
        val log           = streams.value.log
        log.error("♥️ ---- TOTO")
        val cp: Seq[File] = (Compile / internalDependencyClasspath).value.files
        val cp1           = (Compile / externalDependencyClasspath).value.files
        log.error(s"(Compile / internalDependencyClasspath) size: ${cp.size}")
        log.error(s"(Compile / externalDependencyClasspath) size: ${cp1.size}")
        //cp.foreach(f => log.error(s"--- ${f.toString}"))
        log.error(s"🤔️ ---- TOTO")

        val Array(className, fieldName)         = "io.conduktor.api.gql.Gen#api".split("#")
        val classLoader                         = new URLClassLoader((cp ++ cp1).map(_.toURI.toURL).toArray, ClassLoader.getSystemClassLoader)
        val clazz: Either[Throwable, Class[_]]  =
          try Right(classLoader.loadClass(className))
          catch {
            case e: Throwable => Left(e)
          }
        val obzect: Either[Throwable, Class[_]] =
          try Right(classLoader.loadClass(className + "$"))
          catch {
            case e: Throwable => Left(e)
          }

        log.error(s"😭 --- $clazz")
        log.error(s"😭😭 --- $obzect")

        val method   = clazz.toOption.get.getDeclaredMethod(fieldName)
        val instance = obzect.toOption.get.getDeclaredField("MODULE$")
        val api      = method.invoke(instance).asInstanceOf[GraphQL[_]]
        Files.write(
          new File("/Users/jules/conduktor/workspace/scala-gql-template/modules/api/src/main/graphql").toPath,
          api.render.getBytes(StandardCharsets.UTF_8)
        )

        val res = CalibanSourceGenerator(
          calibanSources.value,
          sources.value,
          sourceManaged.value,
          streams.value.cacheDirectory,
          calibanSettings.value.collect { case x: CalibanFileSettings => x },
          calibanSettings.value.collect { case x: CalibanUrlSettings => x }
        )

        log.error(s"♥️ ---- ♥️")
        res.foreach { file =>
          log.error(s"--- $file")
        }
        log.error(s"🤔️ ---- 🤔️️")

        res
      }
    )
  )

  lazy val allSettings = baseSettings ++ calibanScopedSettings

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    CalibanCli.projectSettings ++ inConfig(Compile)(allSettings) ++ inConfig(Test)(allSettings) ++ Seq(
      Compile / sourceGenerators += (Compile / caliban).taskValue,
      Test / sourceGenerators += (Test / caliban).taskValue
    )
}
