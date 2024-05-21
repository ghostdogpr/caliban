package services

import models.{ ID, User }
import zio.{ UIO, ULayer, ZIO, ZLayer }

trait UserService {

  def getUser: UIO[Option[User]]
}

object UserService {
  private val theUser = User(
    averageProductsCreatedPerYear = Some(1337 / 10),
    email = ID("support@apollographql.com"),
    name = Some("Jane Smith"),
    totalProductsCreated = Some(1337),
    yearsOfEmployment = 10
  )

  val inMemory: ULayer[UserService] = ZLayer.succeed(new UserService {
    def getUser: zio.UIO[Option[User]] = ZIO.some(theUser)
  })
}
