package caliban.tools

import zio.{ Task, UIO, ZIO }

import java.io.{ File, PrintWriter }

object Codegen {

  sealed trait GenType

  object GenType {
    object Schema extends GenType

    object Client extends GenType
  }

  def generate(
    loader: SchemaLoader,
    arguments: Options,
    genType: GenType
  ): Task[List[File]] =
    for {
      schema                   <- loader.load
      (packageName, objectName) = getPackageAndObjectName(arguments)
      abstractEffectType        = arguments.abstractEffectType.getOrElse(false)
      effect                    = arguments.effect.getOrElse {
                                    if (abstractEffectType) "F" else "zio.UIO"
                                  }
      preserveInputNames        = arguments.preserveInputNames.getOrElse(false)
      genView                   = arguments.genView.getOrElse(false)
      scalarMappings            = arguments.scalarMappings
      splitFiles                = arguments.splitFiles.getOrElse(false)
      enableFmt                 = arguments.enableFmt.getOrElse(true)
      extensibleEnums           = arguments.extensibleEnums.getOrElse(false)
      code                      = genType match {
                                    case GenType.Schema =>
                                      List(
                                        objectName -> SchemaWriter.write(
                                          schema,
                                          packageName,
                                          effect,
                                          arguments.imports,
                                          scalarMappings,
                                          abstractEffectType,
                                          preserveInputNames
                                        )
                                      )
                                    case GenType.Client =>
                                      ClientWriter.write(
                                        schema,
                                        objectName,
                                        packageName,
                                        genView,
                                        arguments.imports,
                                        splitFiles,
                                        extensibleEnums,
                                        scalarMappings
                                      )
                                  }
      formatted                <- if (enableFmt) Formatter.format(code, arguments.fmtPath) else ZIO.succeed(code)
      paths                    <- ZIO.foreach(formatted) { case (objectName, objectCode) =>
                                    val path =
                                      if (splitFiles) s"${arguments.toPath.reverse.dropWhile(_ != '/').reverse}$objectName.scala"
                                      else arguments.toPath

                                    ZIO.blocking(
                                      ZIO
                                        .attempt(new PrintWriter(new File(path)))
                                        .acquireReleaseWithAuto(pw => ZIO.attempt(pw.print(objectCode)))
                                        .as(new File(path))
                                    )
                                  }
    } yield paths

  def generate(arguments: Options, genType: GenType): Task[List[File]] =
    generate(
      getSchemaLoader(arguments.schemaPath, arguments.headers),
      arguments,
      genType
    )

  private def getSchemaLoader(path: String, schemaPathHeaders: Option[List[Options.Header]]): SchemaLoader =
    if (path.startsWith("http")) SchemaLoader.fromIntrospection(path, schemaPathHeaders)
    else SchemaLoader.fromFile(path)

  def getPackageAndObjectName(arguments: Options): (Option[String], String) = {
    val s           = ".*(?:scala|play.*?|app)[^/]*/(?:(.*)/)?(.*).scala".r.findFirstMatchIn(arguments.toPath)
    val packageName = arguments.packageName.orElse(s.flatMap(x => Option(x.group(1)).map(_.split("/").mkString("."))))
    val objectName  = arguments.clientName.orElse(s.map(_.group(2))).getOrElse("Client")
    packageName -> objectName
  }
}
