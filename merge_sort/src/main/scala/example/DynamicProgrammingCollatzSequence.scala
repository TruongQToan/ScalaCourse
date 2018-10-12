package example

import org.scalameter._
import common._
import java.util.concurrent.atomic._


object DynamicProgrammingCollatzSequence {

  @volatile var dummy = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 50,
    Key.exec.maxWarmupRuns -> 100,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  val maxSize = 5000000

  val table = new AtomicIntegerArray(maxSize)
  table.set(1, 1)

  def collatz(number: Int): Int = {
    var length = 1
    var n = number
    var prev = n
    while (n != 1) {
      if (n % 2 == 0) n /= 2
      else n = 3 * n + 1

      if (n >= table.length || table.get(n) == 0) {
        length += 1
        prev = n
      } else {
        length += table.get(n)
        n = 1
      }
    }

    table.set(number, length)
    length
  }

  def longestCollatz(from: Int, until: Int): Int = {
    (from until until).maxBy(collatz)
  }

  def fjLongestCollatz(from: Int, until: Int, threshold: Int): Int = {
    if (until - from < threshold) {
      longestCollatz(from, until)
    } else {
      val mid = (from + until) / 2
      val right = task {
        fjLongestCollatz(mid, until, threshold)
      }
      val leftLongest = fjLongestCollatz(from, mid, threshold)
      val rightLongest = right.join()
      math.max(leftLongest, rightLongest)
    }
  }

  def main(args: Array[String]): Unit = {
    val until = 100000
    val threshold = 100
    val seqtime = standardConfig measure {
      dummy = longestCollatz(1, until)
    }
    println(s"sequential sum time: $seqtime ms")

    var fjtime = standardConfig measure {
      fjLongestCollatz(1, until, threshold)
    }
    println(s"fork/join time: $fjtime ms")
  }
}
