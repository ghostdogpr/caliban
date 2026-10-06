package caliban.interop.tapir

import caliban.interop.tapir.TestApi.{ File, SomeFieldOutput, UploadedDocument }
import caliban.interop.tapir.TestData._
import caliban.uploads.{ Upload, Uploads }
import zio.stream.{ SubscriptionRef, ZStream }
import zio.{ Hub, Ref, UIO, URIO, ZIO, ZLayer }

import java.math.BigInteger
import java.security.MessageDigest

trait TestService {
  def getCharacters(origin: Option[Origin]): UIO[List[Character]]

  def findCharacter(name: String): UIO[Option[Character]]

  def deleteCharacter(name: String): UIO[Boolean]

  def deletedEvents: ZStream[Any, Nothing, String]

  def awaitDeletedEventsSubscriber: UIO[Unit]

  def reset: UIO[Unit]
}

object TestService {

  def getCharacters(origin: Option[Origin]): URIO[TestService, List[Character]] =
    ZIO.serviceWithZIO(_.getCharacters(origin))

  def findCharacter(name: String): URIO[TestService, Option[Character]] =
    ZIO.serviceWithZIO(_.findCharacter(name))

  def deleteCharacter(name: String): URIO[TestService, Boolean] =
    ZIO.serviceWithZIO(_.deleteCharacter(name))

  def deletedEvents: ZStream[TestService, Nothing, String] =
    ZStream.serviceWithStream(_.deletedEvents)

  def awaitDeletedEventsSubscriber: URIO[TestService, Unit] =
    ZIO.serviceWithZIO(_.awaitDeletedEventsSubscriber)

  def reset: URIO[TestService, Unit] =
    ZIO.serviceWithZIO(_.reset)

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

  def make(initial: List[Character]): ZLayer[Any, Nothing, TestService] = ZLayer {
    for {
      characters  <- Ref.make(initial)
      subscribers <- Hub.unbounded[String]
      subscribed  <- SubscriptionRef.make(0)
    } yield new TestService {

      override def getCharacters(origin: Option[Origin]): UIO[List[Character]] =
        characters.get.map(_.filter(c => origin.forall(c.origin == _)))

      override def findCharacter(name: String): UIO[Option[Character]] = characters.get.map(_.find(c => c.name == name))

      override def deleteCharacter(name: String): UIO[Boolean] =
        characters
          .modify(list =>
            if (list.exists(_.name == name)) (true, list.filterNot(_.name == name))
            else (false, list)
          )
          .tap(deleted => ZIO.when(deleted)(subscribers.publish(name)))

      override def deletedEvents: ZStream[Any, Nothing, String] =
        ZStream.unwrapScoped(
          subscribers.subscribe
            .tap(_ => ZIO.acquireRelease(subscribed.update(_ + 1))(_ => subscribed.update(_ - 1)))
            .map(ZStream.fromQueue(_))
        )

      override def awaitDeletedEventsSubscriber: UIO[Unit] =
        subscribed.changes.takeUntil(_ > 0).runDrain

      override def reset: UIO[Unit] =
        characters.set(initial)
    }
  }

  private def sha256(b: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(b)

  private def hex(b: Array[Byte]): String =
    String.format("%032x", new BigInteger(1, b))
}
