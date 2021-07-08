package caliban.codegen

import _root_.caliban.tools.Codegen.GenType
import _root_.caliban.tools._
import sbt._

import java.io.File
import java.net.URL

object CalibanSourceGenerator {
  import zio._
  import zio.console._

  def transformFile(sourceRoot: File, managedRoot: File, settings: CalibanSettings): File => File = { graphqlFile =>
    val relativePath = settings.packageName.fold(sourceRoot.toPath.relativize(graphqlFile.toPath)) { pkg =>
      val components = pkg.split('.').toList.map(file(_).toPath) :+ graphqlFile.toPath.getFileName
      components.reduceLeft(_.resolve(_))
    }
    val interimPath  = managedRoot.toPath.resolve(relativePath)
    val clientName   = settings.clientName.getOrElse(interimPath.getFileName.toString.stripSuffix(".graphql"))
    val scalaName    = clientName + ".scala"
    interimPath.getParent.resolve(scalaName).toFile
  }

  def collectSettingsFor(fileSettings: Seq[CalibanFileSettings], source: File): CalibanFileSettings =
    // Supply a default packageName.
    // If we do not, `src_managed.main.caliban-codegen-sbt` will be used,
    // which is not only terrible, but invalid.
    CalibanSettings
      .emptyFile(source)
      .packageName("caliban")
      .append(
        fileSettings
          .collect({ case needle if source.toPath.endsWith(needle.file.toPath) => needle })
          .foldLeft[CalibanFileSettings](CalibanSettings.emptyFile(source)) { case (acc, next) =>
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
        opt :: values.map({ case (fst, snd) => s"$fst:$snd" }).mkString(",") :: Nil
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
    val scalarMappings = pairList("--scalarMappings", settings.scalarMappings)
    val imports        = list("--imports", settings.imports)

    scalafmtPath ++ headers ++ packageName ++ genView ++ scalarMappings ++ imports
  }

  def apply(
    sourceRoot: File,
    sources: Seq[File],
    sourceManaged: File,
    cacheDirectory: File,
    fileSettings: Seq[CalibanFileSettings],
    urlSettings: Seq[CalibanUrlSettings]
  ): List[File] = {
    import sbt.util.CacheImplicits._

    def generateSources: List[File] = {
      def generateFileSource(graphql: File, settings: CalibanSettings): IO[Option[Throwable], File] = for {
        generatedSource <- ZIO.succeed(transformFile(sourceRoot, sourceManaged, settings)(graphql))
        _               <- Task(sbt.IO.createDirectory(generatedSource.toPath.getParent.toFile)).asSomeError
        opts            <- ZIO.fromOption(Options.fromArgs(graphql.toString :: generatedSource.toString :: renderArgs(settings)))
        _               <- Codegen.generate(opts, GenType.Client).asSomeError
      } yield new File(opts.toPath)

      def generateUrlSource(graphql: URL, settings: CalibanSettings): IO[Option[Throwable], File] = for {
        generatedSource <-
          ZIO.succeed(
            transformFile(sourceRoot, sourceManaged, settings)(new java.io.File(graphql.getPath.stripPrefix("/")))
          )
        _               <- Task(sbt.IO.createDirectory(generatedSource.toPath.getParent.toFile)).asSomeError
        opts            <- ZIO.fromOption(Options.fromArgs(graphql.toString :: generatedSource.toString :: renderArgs(settings)))
        _               <- Codegen.generate(opts, GenType.Client).asSomeError
      } yield new File(opts.toPath)

      Runtime.default
        .unsafeRun(
          for {
            fromFiles <- ZIO.foreach(sources.toList)(source =>
                           generateFileSource(source, collectSettingsFor(fileSettings, source)).asSome.catchAll {
                             case Some(reason) =>
                               putStrLn(reason.toString) *> putStrLn(reason.getStackTrace.mkString("\n")).as(None)
                             case None         => ZIO.none
                           }
                         )
            fromUrls  <- ZIO.foreach(urlSettings)(setting =>
                           generateUrlSource(setting.url, setting).asSome.catchAll {
                             case Some(reason) =>
                               putStrLn(reason.toString) *> putStrLn(reason.getStackTrace.mkString("\n")).as(None)
                             case None         => ZIO.none
                           }
                         )
          } yield fromFiles ++ fromUrls
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
