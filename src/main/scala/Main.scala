object Main {

  def main(args: Array[String]): Unit = {
    val lhs: Square = Split(Whole(true), Whole(true), Whole(false), Whole(true))
    val rhs: Square = Split(
      Split(Whole(true), Whole(true), Whole(false), Whole(true)),
      Whole(false),
      Whole(true),
      Whole(true)
    )
    println(Square.merge(lhs, rhs))
  }
}
