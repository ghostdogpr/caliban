object Operations {

  final case class Queries(
    characters: zio.UIO[Int]
  )

}
