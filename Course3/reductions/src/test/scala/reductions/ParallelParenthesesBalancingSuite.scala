package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toArray))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toArray))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toArray))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toArray))
  }

  test("mergePairs test => '(((' + ')()'") {
    val p1 = (0, 3)
    val p2 = (1, 0)

    assert(mergePairs(p1, p2) === (0, 2))
  }

  test("mergePairs test => '' + ')()'") {
    val p1 = (0, 0)
    val p2 = (1, 0)

    assert(mergePairs(p1, p2) === (1, 0))
  }

  test("mergePairs test => '()(()()))()' + ')()'") {
    val p1 = (1, 0)
    val p2 = (1, 0)

    assert(mergePairs(p1, p2) === (2, 0))
  }

  test("mergePairs test => ')(((' + '))))))'") {
    val p1 = (1, 3)
    val p2 = (6, 0)

    assert(mergePairs(p1, p2) === (4, 0))
  }

  test("pseudo traverse test") {
    val idx = 0
    val chars = ")((()("
    val until = chars.length

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

    assert((right, left) === (1, 3))
  }

  test("parBalance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 2) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("parBalance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("parBalance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(parBalance("(if (zero? x) max (/ 1 x))".toArray, 3))
  }

  test("parBalance: 'I told him ...' is balanced") {
    assert(parBalance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toArray, 5))
  }

  test("parBalance: ':-)' is unbalanced") {
    assert(!parBalance(":-)".toArray, 2))
  }

  test("parBalance: counting is not enough") {
    assert(!parBalance("())(".toArray, 2))
  }

}