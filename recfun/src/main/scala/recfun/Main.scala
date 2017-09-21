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
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def balanceHelper(charList: List[Char], bracketCount: Int): Boolean = charList match {
        case _ if bracketCount < 0 => false
        case h :: t if h == '(' => balanceHelper(t, bracketCount + 1)
        case h :: t if h == ')' => balanceHelper(t, bracketCount - 1)
        case _ :: t => balanceHelper(t, bracketCount)
        case Nil => bracketCount == 0
      }
      balanceHelper(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def changeHelper(currentTotal: Int, coins: List[Int]): Int = coins match {
        case Nil => 0
        case _ if currentTotal > money => 0
        case _ if currentTotal == money => 1;
        case h :: t if currentTotal < money => changeHelper(currentTotal + h, coins) + changeHelper(currentTotal, t)
      }
      changeHelper(0, coins)
    }
  }
