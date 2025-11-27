package caliban.codegen

import zio.{ Task, ZIO }

import java.io.{ File, PrintWriter }
import caliban.tools._

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
      addDerives                = arguments.addDerives.getOrElse(false)
      envForDerives             = arguments.envForDerives
      genView                   = arguments.genView.getOrElse(false)
      scalarMappings            = arguments.scalarMappings
      splitFiles                = arguments.splitFiles.getOrElse(false)
      enableFmt                 = arguments.enableFmt.getOrElse(true)
      extensibleEnums           = arguments.extensibleEnums.getOrElse(false)
      excludeDeprecated         = arguments.excludeDeprecated.getOrElse(false)
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
                                          preserveInputNames,
                                          addDerives,
                                          envForDerives
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
                                        scalarMappings,
                                        excludeDeprecated
                                      )
                                  }
      formatted                <- if (enableFmt) Formatter.format(code, arguments.fmtPath) else ZIO.succeed(code)
      paths                    <- ZIO.foreach(formatted) { case (objectName, objectCode) =>
                                    ZIO.attemptBlocking {
                                      val f = new File(arguments.toPath)
                                      if (splitFiles)
                                        new File(
                                          if (f.isDirectory) f    // directory exists => use it
                                          else if (!f.exists()) { // directory does not exist => create it
                                            f.mkdirs()
                                            f
                                          } else f.getParentFile, // file exists => use its parent
                                          s"$objectName.scala"
                                        )
                                      else f
                                    }.flatMap(file =>
                                      ZIO.blocking(
                                        ZIO
                                          .attempt(new PrintWriter(file))
                                          .acquireReleaseWithAuto(pw => ZIO.attempt(pw.print(objectCode)))
                                          .as(file)
                                      )
                                    )
                                  }
    } yield paths

  def generate(arguments: Options, genType: GenType): Task[List[File]] =
    generate(
      getSchemaLoader(
        arguments.schemaPath,
        arguments.headers, {
          val default = IntrospectionClient.Config.default
          IntrospectionClient.Config(
            supportDeprecatedArgs = arguments.supportDeprecatedArgs.getOrElse(default.supportDeprecatedArgs),
            supportIsRepeatable = arguments.supportIsRepeatable.getOrElse(default.supportIsRepeatable)
          )
        }
      ),
      arguments,
      genType
    )

  private def getSchemaLoader(
    path: String,
    schemaPathHeaders: Option[List[Header]],
    config: IntrospectionClient.Config
  ): SchemaLoader =
    if (path.startsWith("http")) SchemaLoader.fromIntrospection(path, schemaPathHeaders, config)
    else SchemaLoader.fromFile(path)

  def getPackageAndObjectName(arguments: Options): (Option[String], String) = {
    val s           = ".*(?:scala|play.*?|app)[^/]*/(?:(.*)/)?(.*).scala".r.findFirstMatchIn(arguments.toPath)
    val packageName = arguments.packageName.orElse(s.flatMap(x => Option(x.group(1)).map(_.split("/").mkString("."))))
    val objectName  = arguments.clientName.orElse(s.map(_.group(2))).getOrElse("Client")
    packageName -> objectName
  }
}
