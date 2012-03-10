package utils

object Combinatorics {

  /** flatMapSublists is like list.flatMap, but instead of passing each element
   *  to the function, it passes successive sublists of L.
   */
  private def flatMapSublists[A, B](ls: Seq[A])(f: (Seq[A]) => Seq[B]): Seq[B] =
    ls match {
      case Nil                   => Nil
      case sublist @ (_ :: tail) => f(sublist) ++: flatMapSublists(tail)(f)
    }

  /** Generate the combinations of K distinct objects chosen from the N
   *  elements of a list.
   *  In how many ways can a committee of 3 be chosen from a group of 12
   *  people?  We all know that there are C(12,3) = 220 possibilities (C(N,K)
   *  denotes the well-known binomial coefficient).  For pure mathematicians,
   *  this result may be great.  But we want to really generate all the possibilities.
   *
   *  Example:
   *  scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
   *  res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
   */
  def combinations[A](n: Int, ls: Seq[A]): Seq[Seq[A]] =
    if (n == 0) Seq(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map { sl.head +: _ }
    }

}