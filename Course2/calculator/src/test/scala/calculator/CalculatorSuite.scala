package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }


  test("computeDelta") {
    val a = Var(1.0)
    val b = Var(2.0)
    val c = Var(3.0)

    val result1 = Polynomial.computeDelta(a, b, c)
    assert(-8.0 == result1())

    b() = 5.0
    assert(13.0 == result1())
  }

  test("computeSolutions") {
    val a = Var(1.0)
    val b = Var(0.0)
    val c = Var(0.0)

    val delta = Polynomial.computeDelta(a, b, c)
    val result = Polynomial.computeSolutions(a, b, c, delta)
    assert(Set(0.0) == result())

    c() = -25.0
    assert(Set(5.0, -5.0) == result())

    c() = 25.0
    assert(Set() == result())
  }

  test("test computeValues ok") {
    val equations: Map[String, Signal[Expr]] = Map("a" -> Signal(Ref("b")), "b" -> Signal(Literal(2)))
    val maybeSignal: Option[Signal[Double]] = Calculator.computeValues(equations).get("a")
    val answer = maybeSignal match {
      case Some(exp) => exp()
      case _ => Double.NaN
    }

    assert(2.0 == answer)
  }

  test("test computeValues fail") {
    val equations: Map[String, Signal[Expr]] = Map("a" -> Signal(Ref("b")), "b" -> Signal(Ref("c")), "c" -> Signal(Ref("a")))
    val maybeSignal: Option[Signal[Double]] = Calculator.computeValues(equations).get("a")
    val answer = maybeSignal match {
      case Some(exp) => exp()
      case _ => new Exception
    }

    assert(answer equals Double.NaN)
  }
}
