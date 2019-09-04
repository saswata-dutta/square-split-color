sealed trait Square
final case class Split(s1: Square, s2: Square, s3: Square, s4: Square) extends Square
final case class Whole(isBlack: Boolean) extends Square

object Square {

  def merge(lhs: Square, rhs: Square): Square =
    (lhs, rhs) match {
      case (a: Whole, b: Whole) => Whole(a.isBlack || b.isBlack)
      case (a: Whole, b: Split) => mergeWhole(a, b)
      case (b: Split, a: Whole) => mergeWhole(a, b)
      case (a: Split, b: Split) => mergeSplit(a, b)
    }

  def mergeWhole(a: Whole, b: Split): Square =
    coalesce(Split(merge(a, b.s1), merge(a, b.s2), merge(a, b.s3), merge(a, b.s4)))

  def mergeSplit(a: Split, b: Split): Square =
    coalesce(Split(merge(a.s1, b.s1), merge(a.s2, b.s2), merge(a.s3, b.s3), merge(a.s4, b.s4)))

  def coalesce(s: Split): Square =
    s match {
      case Split(Whole(a), Whole(b), Whole(c), Whole(d)) if a == b && a == c && a == d => Whole(a)
      case _                                                                           => s
    }
}
