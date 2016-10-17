package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) = 
      assert(countChange(money, coins) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) = 
      assert(countChange(0, coins) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) = 
      assert(countChange(money, List()) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("moneyThreshold test") {
    val unimportant: List[Int] = List(1, 2, 3)

    assert(moneyThreshold(3)(2, unimportant) === true)
    assert(moneyThreshold(2)(2, unimportant) === false)
    assert(moneyThreshold(4)(2, unimportant) === true)
  }

  test("parCountChange should return 0 for money < 0 with moneyThreshold") {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChange(money, coins, moneyThreshold(money)) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0 with moneyThreshold") {
    def check(coins: List[Int]) =
      assert(parCountChange(0, coins, moneyThreshold(0)) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List() with moneyThreshold") {
    def check(money: Int) =
      assert(parCountChange(money, List(), moneyThreshold(money)) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin with moneyThreshold") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, moneyThreshold(money)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("parCountChange should work for multi-coins with moneyThreshold") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, moneyThreshold(money)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("totalCoinsThreshold test") {
    val unimportant = 2
    val coins: List[Int] = List(1, 2, 3)

    assert(totalCoinsThreshold(3)(unimportant, coins) === false)
    assert(totalCoinsThreshold(1)(unimportant, coins) === false)
    assert(totalCoinsThreshold(5)(unimportant, coins) === true)
  }

  test("parCountChange should return 0 for money < 0 with totalCoinsThreshold") {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChange(money, coins, totalCoinsThreshold(coins.length)) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0 with totalCoinsThreshold") {
    def check(coins: List[Int]) =
      assert(parCountChange(0, coins, totalCoinsThreshold(coins.length)) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List() with totalCoinsThreshold") {
    def check(money: Int) =
      assert(parCountChange(money, List(), totalCoinsThreshold(0)) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin with totalCoinsThreshold") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, totalCoinsThreshold(coins.length)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("parCountChange should work for multi-coins with totalCoinsThreshold") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, totalCoinsThreshold(coins.length)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("combinedThreshold test") {
    val money = 2
    val coins: List[Int] = List(1, 2, 3)

    assert(combinedThreshold(3, List(1, 1, 1))(money, coins) === false)
    assert(combinedThreshold(4, List(1, 1, 1))(money, coins) === true)
    assert(combinedThreshold(4, List(1, 1, 1, 1))(money, coins) === true)
  }

  test("parCountChange should return 0 for money < 0 with combinedThreshold") {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0 with combinedThreshold") {
    def check(coins: List[Int]) =
      assert(parCountChange(0, coins, combinedThreshold(0, coins)) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List() with combinedThreshold") {
    def check(money: Int) =
      assert(parCountChange(money, List(), combinedThreshold(money, List())) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin with combinedThreshold") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("parCountChange should work for multi-coins with combinedThreshold") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }
}
