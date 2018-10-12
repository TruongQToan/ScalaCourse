package calculator

import scala.util.Try

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
        namedExpressions map { case (key, value) => (key, Signal { eval(value(), namedExpressions) } )}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    val dependency = Set[String]()
    def evalRec(expr: Expr, references: Map[String, Signal[Expr]], dependency: Set[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) => {
          if (dependency contains name) Double.NaN
          else evalRec(getReferenceExpr(name, references), references, dependency + name)
        }
        case Plus(a, b) => evalRec(a, references, dependency) + evalRec(b, references, dependency)
        case Minus(a, b) => evalRec(a, references, dependency) - evalRec(b, references, dependency)
        case Times(a, b) => evalRec(a, references, dependency) * evalRec(b, references, dependency)
        case Divide(a, b) => Try(evalRec(a, references, dependency) / evalRec(b, references, dependency)).getOrElse(Double.NaN)
        case _ => Double.NaN
      }
    }
    evalRec(expr, references, dependency)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
