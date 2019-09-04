object Main {

  def main(args: Array[String]): Unit = {
    val lhs: Square = Split(White, Split(Black, White, Black, White), White, Black)
    val rhs: Square =
      Split(Split(Black, Black, Black, Black), Split(White, Black, Black, Black), Black, Black)
    println(Square.merge(lhs, rhs))
  }
}
