package caliban.codegen

import sbt._
import sbt.Keys._

import _root_.caliban.tools.Codegen.GenType
import _root_.caliban.tools._
import _root_.caliban.tools.implicits.ScalarMappings

import java.io.{ File, PrintWriter }

object CalibanSourceGenerator {
  import zio._
  import zio.console._

  def transformFile(sourceRoot: File, managedRoot: File, settings: CalibanSettings): File => File = { graphqlFile =>
    val relativePath = settings.packageName.fold(sourceRoot.toPath.relativize(graphqlFile.toPath)) { pkg =>
      val components = pkg.split('.').toList.map(file(_).toPath) :+ graphqlFile.toPath.getFileName()
      components.reduceLeft(_.resolve(_))
    }
    val interimPath  = managedRoot.toPath.resolve(relativePath)
    val clientName   = settings.clientName.getOrElse(interimPath.getFileName().toString().stripSuffix(".graphql"))
    val scalaName    = clientName + ".scala"
    interimPath.getParent.resolve(scalaName).toFile
  }

  def apply(
    sourceRoot: File,
    sources: Seq[File],
    sourceManaged: File,
    cacheDirectory: File,
    settings: Seq[(File, CalibanSettings)]
  ): List[File] = {
    import sbt.util.CacheImplicits._

    def collectSettingsFor(source: File): CalibanSettings =
      // Supply a default packageName.
      // If we do not, `src_managed.main.caliban-codegen-sbt` will be used,
      // which is not only terrible, but invalid.
      CalibanSettings.empty
        .packageName("caliban")
        .append(
          settings
            .collect({ case (f, needle) if source.toPath.endsWith(f.toPath) => needle })
            .foldLeft(CalibanSettings.empty) { case (acc, next) =>
              acc.append(next)
            }
        )

    def renderArgs(settings: CalibanSettings): List[String] = {
      def singleOpt(opt: String, value: Option[String]): List[String]        =
        if (value.nonEmpty) {
          opt :: value.toList
        } else Nil
      def pairList(opt: String, values: Seq[(String, String)]): List[String] =
        if (values.nonEmpty) {
          opt :: values.map({ case (fst, snd) => s"${fst}:${snd}" }).mkString(",") :: Nil
        } else Nil
      def list(opt: String, values: Seq[String]): List[String]               =
        if (values.nonEmpty) {
          opt :: values.mkString(",") :: Nil
        } else Nil
      val scalafmtPath                                                       = singleOpt("--scalafmtPath", settings.scalafmtPath)
      val headers                                                            = pairList("--headers", settings.headers)
      val packageName                                                        = singleOpt(
        "--packageName",
        settings.packageName
      )

      val genView = singleOpt(
        "--genView",
        settings.genView.map(_.toString())
      ) // NB: Presuming zio-config can read toString'd booleans
      val effect         = singleOpt("--effect", settings.effect)
      val scalarMappings = pairList("--scalarMappings", settings.scalarMappings)
      val imports        = list("--imports", settings.imports)

      scalafmtPath ++ headers ++ packageName ++ genView ++ effect ++ scalarMappings ++ imports
    }

    def generateSources: List[File] = {
      def generateSource(graphql: File, settings: CalibanSettings): IO[Option[Throwable], File] = for {
        generatedSource <- ZIO.succeed(transformFile(sourceRoot, sourceManaged, settings)(graphql))
        _               <- Task(sbt.IO.createDirectory(generatedSource.toPath.getParent.toFile)).asSomeError
        opts            <- ZIO.fromOption(Options.fromArgs(graphql.toString :: generatedSource.toString :: renderArgs(settings)))
        res             <- Codegen.generate(opts, GenType.Client).asSomeError
      } yield new File(opts.toPath)

      Runtime.default
        .unsafeRun(
          ZIO.foreach(sources.toList)(source =>
            generateSource(source, collectSettingsFor(source)).asSome.catchAll {
              case Some(reason) => putStrLn(reason.toString) *> putStrLn(reason.getStackTrace.mkString("\n")).as(None)
              case None         => ZIO.none
            }
          )
        )
        .flatten
    }

    // NB: This is heavily inspired by the caching technique from eed3si9n's sbt-scalaxb plugin
    def cachedGenerateSources =
      Tracked.inputChanged(cacheDirectory / "caliban-inputs") {
        (inChanged, _: (List[File], FilesInfo[ModifiedFileInfo], String)) =>
          Tracked.outputChanged(cacheDirectory / "caliban-output") { (outChanged, outputs: FilesInfo[PlainFileInfo]) =>
            if (inChanged || outChanged) generateSources
            else outputs.files.toList.map(_.file)
          }
      }

    def inputs: (List[File], FilesInfo[ModifiedFileInfo], String) =
      (
        sources.toList,
        FilesInfo.lastModified(sources.toSet).asInstanceOf[FilesInfo[ModifiedFileInfo]],
        BuildInfo.version
      )

    cachedGenerateSources(inputs)(() =>
      FilesInfo.exists((sourceManaged ** "*.scala").get.toSet).asInstanceOf[FilesInfo[PlainFileInfo]]
    )
  }
}
