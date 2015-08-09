package recfun
import common._

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
    def isEdge = {
      c == 0 || c == r
    }
    
    
    if (isEdge) 1
    else pascal( c-1, r-1) + pascal( c, r-1)
  }
  

  /**
   * Exercise 2
   */
  
  
  
  def balance(chars: List[Char]): Boolean = {
    
    def balanceAcc(chars : List[Char], openBracetsCount:Int):Boolean = {
        if (openBracetsCount < 0) false
        else if (chars.isEmpty) true
        else if ( chars.head.equals('(') ) balanceAcc(chars.tail, openBracetsCount + 1)
        else if ( chars.head.equals(')') ) balanceAcc(chars.tail, openBracetsCount -1)
        else balanceAcc(chars.tail, openBracetsCount)
    }

    return balanceAcc(chars, 0)
  }
  

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
