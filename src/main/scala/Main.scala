import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    val lhs: Square = Split(White, Split(Black, White, Black, White), White, Black)
    val rhs: Square =
      Split(Split(Black, Black, Black, Black), Split(White, Black, Black, Black), Black, Black)
    println(Square.merge(lhs, rhs))

    val input = "( (wwww) b (bbbb) ( w (bwbw) (bbw (wwbb)) b ) )"
    println(parse(input))
  }

  def parse(input: String): Square = {
    import Square.coalesce

    val tokens =
      input.toUpperCase
        .split("")
        .map(_.head)
        .filterNot(_.isWhitespace)

    val stack = mutable.Stack[Any]()

    tokens.foreach {
      case '(' => stack.push('(')
      case 'B' => stack.push(Black)
      case 'W' => stack.push(White)
      case ')' =>
        assert(stack.size >= 5, "Insufficient items in stack")
        val s4 = stack.pop()
        val s3 = stack.pop()
        val s2 = stack.pop()
        val s1 = stack.pop()
        assert('(' == stack.pop(), "Bracket mismatched")
        (s1, s2, s3, s4) match {
          case (a: Square, b: Square, c: Square, d: Square) =>
            stack.push(coalesce(Split(a, b, c, d)))
          case _ => throw new IllegalArgumentException(s"Bad Input $s1 $s2 $s3 $s4")
        }
    }

    assert(stack.size == 1, "Unbalanced Input")
    stack.pop() match {
      case s: Square => s
      case x         => throw new IllegalArgumentException(s"Bad Input $x")
    }
  }
}
