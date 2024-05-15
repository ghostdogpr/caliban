import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Big
  object Big {

    final case class BigView[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ](
      user1: User1Selection,
      user2: User2Selection,
      user3: User3Selection,
      user4: User4Selection,
      user5: User5Selection,
      user6: User6Selection,
      user7: User7Selection,
      user8: User8Selection,
      user9: User9Selection,
      user10: User10Selection,
      user11: User11Selection,
      user12: User12Selection,
      user13: User13Selection,
      user14: User14Selection,
      user15: User15Selection,
      user16: User16Selection,
      user17: User17Selection,
      user18: User18Selection,
      user19: User19Selection,
      user20: User20Selection,
      user21: User21Selection,
      user22: User22Selection,
      user23: User23Selection
    )

    final case class BigViewSelectionArgs[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ](
      user1Selection: SelectionBuilder[User, User1Selection],
      user2Selection: SelectionBuilder[User, User2Selection],
      user3Selection: SelectionBuilder[User, User3Selection],
      user4Selection: SelectionBuilder[User, User4Selection],
      user5Selection: SelectionBuilder[User, User5Selection],
      user6Selection: SelectionBuilder[User, User6Selection],
      user7Selection: SelectionBuilder[User, User7Selection],
      user8Selection: SelectionBuilder[User, User8Selection],
      user9Selection: SelectionBuilder[User, User9Selection],
      user10Selection: SelectionBuilder[User, User10Selection],
      user11Selection: SelectionBuilder[User, User11Selection],
      user12Selection: SelectionBuilder[User, User12Selection],
      user13Selection: SelectionBuilder[User, User13Selection],
      user14Selection: SelectionBuilder[User, User14Selection],
      user15Selection: SelectionBuilder[User, User15Selection],
      user16Selection: SelectionBuilder[User, User16Selection],
      user17Selection: SelectionBuilder[User, User17Selection],
      user18Selection: SelectionBuilder[User, User18Selection],
      user19Selection: SelectionBuilder[User, User19Selection],
      user20Selection: SelectionBuilder[User, User20Selection],
      user21Selection: SelectionBuilder[User, User21Selection],
      user22Selection: SelectionBuilder[User, User22Selection],
      user23Selection: SelectionBuilder[User, User23Selection]
    )

    type ViewSelection[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ] = SelectionBuilder[Big, BigView[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ]]

    def view[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ](
      selectionArgs: BigViewSelectionArgs[
        User1Selection,
        User2Selection,
        User3Selection,
        User4Selection,
        User5Selection,
        User6Selection,
        User7Selection,
        User8Selection,
        User9Selection,
        User10Selection,
        User11Selection,
        User12Selection,
        User13Selection,
        User14Selection,
        User15Selection,
        User16Selection,
        User17Selection,
        User18Selection,
        User19Selection,
        User20Selection,
        User21Selection,
        User22Selection,
        User23Selection
      ]
    ): ViewSelection[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ] = (((user1(selectionArgs.user1Selection) ~ user2(selectionArgs.user2Selection) ~ user3(
      selectionArgs.user3Selection
    ) ~ user4(selectionArgs.user4Selection) ~ user5(selectionArgs.user5Selection) ~ user6(
      selectionArgs.user6Selection
    ) ~ user7(selectionArgs.user7Selection) ~ user8(selectionArgs.user8Selection) ~ user9(
      selectionArgs.user9Selection
    ) ~ user10(selectionArgs.user10Selection) ~ user11(selectionArgs.user11Selection) ~ user12(
      selectionArgs.user12Selection
    ) ~ user13(selectionArgs.user13Selection) ~ user14(selectionArgs.user14Selection) ~ user15(
      selectionArgs.user15Selection
    ) ~ user16(selectionArgs.user16Selection) ~ user17(selectionArgs.user17Selection) ~ user18(
      selectionArgs.user18Selection
    ) ~ user19(selectionArgs.user19Selection) ~ user20(selectionArgs.user20Selection) ~ user21(
      selectionArgs.user21Selection
    ) ~ user22(selectionArgs.user22Selection)) ~ (user23(selectionArgs.user23Selection)))).map {
      case (
            (
              user1,
              user2,
              user3,
              user4,
              user5,
              user6,
              user7,
              user8,
              user9,
              user10,
              user11,
              user12,
              user13,
              user14,
              user15,
              user16,
              user17,
              user18,
              user19,
              user20,
              user21,
              user22
            ),
            (user23)
          ) =>
        BigView(
          user1,
          user2,
          user3,
          user4,
          user5,
          user6,
          user7,
          user8,
          user9,
          user10,
          user11,
          user12,
          user13,
          user14,
          user15,
          user16,
          user17,
          user18,
          user19,
          user20,
          user21,
          user22,
          user23
        )
    }

    def user1[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user1", Obj(innerSelection))
    def user2[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user2", Obj(innerSelection))
    def user3[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user3", Obj(innerSelection))
    def user4[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user4", Obj(innerSelection))
    def user5[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user5", Obj(innerSelection))
    def user6[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user6", Obj(innerSelection))
    def user7[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user7", Obj(innerSelection))
    def user8[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user8", Obj(innerSelection))
    def user9[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user9", Obj(innerSelection))
    def user10[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user10", Obj(innerSelection))
    def user11[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user11", Obj(innerSelection))
    def user12[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user12", Obj(innerSelection))
    def user13[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user13", Obj(innerSelection))
    def user14[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user14", Obj(innerSelection))
    def user15[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user15", Obj(innerSelection))
    def user16[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user16", Obj(innerSelection))
    def user17[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user17", Obj(innerSelection))
    def user18[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user18", Obj(innerSelection))
    def user19[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user19", Obj(innerSelection))
    def user20[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user20", Obj(innerSelection))
    def user21[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user21", Obj(innerSelection))
    def user22[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user22", Obj(innerSelection))
    def user23[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user23", Obj(innerSelection))
  }

  type User
  object User {

    final case class UserView(
      character1: String,
      character2: String,
      character3: String,
      character4: String,
      character5: String,
      character6: String,
      character7: String,
      character8: String,
      character9: String,
      character10: String,
      character11: String,
      character12: String,
      character13: String,
      character14: String,
      character15: String,
      character16: String,
      character17: String,
      character18: String,
      character19: String,
      character20: String,
      character21: String,
      character22: String,
      character23: String
    )

    final case class UserViewArgs(
      character1Name: String,
      character2Name: String,
      character3Name: String,
      character4Name: String,
      character5Name: String,
      character6Name: String,
      character7Name: String,
      character8Name: String,
      character9Name: String,
      character10Name: String,
      character11Name: String,
      character12Name: String,
      character13Name: String,
      character14Name: String,
      character15Name: String,
      character16Name: String,
      character17Name: String,
      character18Name: String,
      character19Name: String,
      character20Name: String,
      character21Name: String,
      character22Name: String,
      character23Name: String
    )

    type ViewSelection = SelectionBuilder[User, UserView]

    def view(args: UserViewArgs): ViewSelection =
      (((character1(args.character1Name) ~ character2(args.character2Name) ~ character3(
        args.character3Name
      ) ~ character4(args.character4Name) ~ character5(args.character5Name) ~ character6(
        args.character6Name
      ) ~ character7(args.character7Name) ~ character8(args.character8Name) ~ character9(
        args.character9Name
      ) ~ character10(args.character10Name) ~ character11(args.character11Name) ~ character12(
        args.character12Name
      ) ~ character13(args.character13Name) ~ character14(args.character14Name) ~ character15(
        args.character15Name
      ) ~ character16(args.character16Name) ~ character17(args.character17Name) ~ character18(
        args.character18Name
      ) ~ character19(args.character19Name) ~ character20(args.character20Name) ~ character21(
        args.character21Name
      ) ~ character22(args.character22Name)) ~ (character23(args.character23Name)))).map {
        case (
              (
                character1,
                character2,
                character3,
                character4,
                character5,
                character6,
                character7,
                character8,
                character9,
                character10,
                character11,
                character12,
                character13,
                character14,
                character15,
                character16,
                character17,
                character18,
                character19,
                character20,
                character21,
                character22
              ),
              (character23)
            ) =>
          UserView(
            character1,
            character2,
            character3,
            character4,
            character5,
            character6,
            character7,
            character8,
            character9,
            character10,
            character11,
            character12,
            character13,
            character14,
            character15,
            character16,
            character17,
            character18,
            character19,
            character20,
            character21,
            character22,
            character23
          )
      }

    def character1(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character1", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character2(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character2", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character3(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character3", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character4(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character4", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character5(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character5", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character6(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character6", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character7(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character7", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character8(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character8", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character9(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character9", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character10(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character10", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character11(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character11", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character12(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character12", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character13(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character13", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character14(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character14", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character15(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character15", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character16(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character16", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character17(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character17", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character18(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character18", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character19(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character19", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character20(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character20", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character21(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character21", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character22(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character22", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character23(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character23", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
  }

}
