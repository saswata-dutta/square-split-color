sealed trait Square
// each square is solid black/white or split into 4 quads
final case class Split(s1: Square, s2: Square, s3: Square, s4: Square) extends Square
case object Black extends Square
case object White extends Square

object Square {

  def merge(lhs: Square, rhs: Square): Square =
    (lhs, rhs) match {
      case (Black, _)           => Black
      case (_, Black)           => Black
      case (White, other)       => coalesce(other)
      case (other, White)       => coalesce(other)
      case (a: Split, b: Split) => mergeSplit(a, b)
    }

  def mergeSplit(a: Split, b: Split): Square =
    coalesce(Split(merge(a.s1, b.s1), merge(a.s2, b.s2), merge(a.s3, b.s3), merge(a.s4, b.s4)))

  def coalesce(s: Square): Square =
    s match {
      case Split(Black, Black, Black, Black) => Black
      case Split(White, White, White, White) => White
      case _                                 => s
    }
}
