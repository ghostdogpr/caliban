package poc.caliban.logrocket

import poc.caliban.logrocket.GraphQLApi._
import zio.ZIO

object LogrocketService {
  def userRegister(input: MutationUserRegisterArgs): ZEff[UserRegisterResultSuccess] = ZIO.succeed {
    UserRegisterResultSuccess(User(id = "1", login = "test"))
  }
}
