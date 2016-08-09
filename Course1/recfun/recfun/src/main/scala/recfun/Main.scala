package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    factorial(r) / (factorial(r-c) * factorial(c))
  }


  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if(n < 0) 1
      else if(n == 0) acc
      else go(n-1, acc * n)
    }

    go(n, 1)
  }
  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars: List[Char], p: List[Char]): Boolean = {
      chars match {
        case Nil => p.isEmpty
        case head :: tail =>
          if (head == '(') loop(tail, p :+ head)
          else if (head == ')')
            if (p.isEmpty) false
            else loop(tail, p.tail)
          else loop(tail, p)
      }
    }

    loop(chars, List())
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def pay(remain: Int, coins: List[Int]): Int = {
      if (remain < 0) 0
      else if (remain == 0) 1
      else coins match {
        case Nil => 0
        case head :: tail => pay(remain - head, coins) + pay(remain, tail)
      }
    }

    pay(money, coins)
  }
}
