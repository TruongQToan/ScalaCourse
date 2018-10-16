package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i = 0
    var counter = 0
    while (i < chars.length) {
      if (chars(i) == ')') {
        if (counter == 0) {
          counter -= 1
          i = chars.length // break
        }
        else if (counter > 0) counter -= 1
      } else if (chars(i) == '(') counter += 1
      i += 1
    }
    counter == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int): (Int, Int) /*: ???*/ = {
      var i = from
      var leftCounter = 0
      var rightCounter = 0
      while (i < until) {
        //println(i, from, until)
        if (chars(i) == ')') {
          if (leftCounter == 0) rightCounter += 1
          else if (leftCounter > 0) leftCounter -= 1
        } else if (chars(i) == '(') leftCounter += 1
        i += 1
      }
      (leftCounter, rightCounter)
    }

    def reduce(from: Int, until: Int): (Int, Int) /*: ???*/ = {
      if (until - from <= threshold) traverse(from, until)
      else {
        val mid = (until - from) / 2 + from
        val (x, y) = parallel(reduce(from, mid), reduce(mid, until))
        if (x._1 < y._2) {
          (y._1, x._2 + y._2 - x._1)
        } else {
          (x._1 - y._2 + y._1, x._2)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
