package caliban.interop.tapir

import caliban.interop.tapir.TestApi.{ File, SomeFieldOutput, UploadedDocument }
import caliban.interop.tapir.TestData._
import caliban.uploads.{ Upload, Uploads }
import zio.stream.ZStream
import zio.{ Has, Hub, Ref, UIO, URIO, ZIO, ZLayer }

import java.math.BigInteger
import java.security.MessageDigest

object TestService {

  type TestService = Has[Service]

  trait Service {
    def getCharacters(origin: Option[Origin]): UIO[List[Character]]

    def findCharacter(name: String): UIO[Option[Character]]

    def deleteCharacter(name: String): UIO[Boolean]

    def deletedEvents: ZStream[Any, Nothing, String]
  }

  def getCharacters(origin: Option[Origin]): URIO[TestService, List[Character]] =
    URIO.serviceWith(_.getCharacters(origin))

  def findCharacter(name: String): URIO[TestService, Option[Character]] =
    URIO.serviceWith(_.findCharacter(name))

  def deleteCharacter(name: String): URIO[TestService, Boolean] =
    URIO.serviceWith(_.deleteCharacter(name))

  def deletedEvents: ZStream[TestService, Nothing, String] =
    ZStream.accessStream(_.get.deletedEvents)

  def uploadFile(file: Upload): ZIO[Uploads, Throwable, File] =
    for {
      bytes <- file.allBytes
      meta  <- file.meta
    } yield File(
      hex(sha256(bytes.toArray)),
      meta.map(_.fileName).getOrElse(""),
      meta.flatMap(_.contentType).getOrElse("")
    )

  def uploadFiles(files: List[Upload]): ZIO[Uploads, Throwable, List[File]] =
    ZIO.collectAllPar(
      for {
        file <- files
      } yield for {
        bytes <- file.allBytes
        meta  <- file.meta
      } yield File(
        hex(sha256(bytes.toArray)),
        meta.map(_.fileName).getOrElse(""),
        meta.flatMap(_.contentType).getOrElse("")
      )
    )

  def uploadFilesWithOtherFields(
    uploadedDocuments: List[UploadedDocument]
  ): ZIO[Uploads, Throwable, List[SomeFieldOutput]] =
    ZIO.succeed(
      for {
        document <- uploadedDocuments
      } yield SomeFieldOutput(document.someField1, document.someField2)
    )

  def make(initial: List[Character]): ZLayer[Any, Nothing, TestService] =
    (for {
      characters  <- Ref.make(initial)
      subscribers <- Hub.unbounded[String]
    } yield new Service {

      def getCharacters(origin: Option[Origin]): UIO[List[Character]] =
        characters.get.map(_.filter(c => origin.forall(c.origin == _)))

      def findCharacter(name: String): UIO[Option[Character]] = characters.get.map(_.find(c => c.name == name))

      def deleteCharacter(name: String): UIO[Boolean] =
        characters
          .modify(list =>
            if (list.exists(_.name == name)) (true, list.filterNot(_.name == name))
            else (false, list)
          )
          .tap(deleted => UIO.when(deleted)(subscribers.publish(name)))

      def deletedEvents: ZStream[Any, Nothing, String] =
        ZStream.unwrapManaged(subscribers.subscribe.map(ZStream.fromQueue(_)))
    }).toLayer

  private def sha256(b: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(b)

  private def hex(b: Array[Byte]): String =
    String.format("%032x", new BigInteger(1, b))
}
