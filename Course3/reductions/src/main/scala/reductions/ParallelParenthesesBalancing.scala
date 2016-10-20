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
    @tailrec
    def loop(idx: Int, until: Int, count: Int): Boolean = {
      if (idx == until) count == 0
      else{
        val c = chars(idx)

        if (c == '(') loop(idx + 1, until, count + 1)
        else if (c == ')')
          if (count <= 0) false
          else loop(idx + 1, until, count - 1)
        else loop(idx + 1, until, count)
      }
    }

    loop(0, chars.length, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var right = 0
      var left = 0

      var i = idx
      while(i < until){
        val c = chars(i)

        if (c == '(') left += 1
        else if (c == ')'){
          if(left > 0) left -= 1
          else right += 1
        }

        i += 1
      }

      (right, left)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (p1, p2) = parallel(reduce(from, mid), reduce(mid, until))

        mergePairs(p1, p2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  def mergePairs(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = {
    val (r1, l1) = p1
    val (r2, l2) = p2

    if(l1 > r2) (r1, l1 - r2 + l2)
    else (r1 + r2 - l1, l2)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
