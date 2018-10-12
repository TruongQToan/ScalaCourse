object polynomials {

  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)):(Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p1 = new Poly(1->2.0, 3->4.0, 5->6.0)
  val p2 = new Poly(0->3.0, 3->7.0)
  p1 + p2

  def wordOccurrences(w: String): List[(Char, Int)] =
    w.toLowerCase.toList.groupBy((c : Char) => c).toList.map(xy => (xy._1, xy._2.length)).sorted

  wordOccurrences("abcccda")
}
