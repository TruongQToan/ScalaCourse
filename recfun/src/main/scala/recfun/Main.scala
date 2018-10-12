package recfun

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
    def pascal(c: Int, r: Int): Int = 
        if (c == 0 || r == c) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def rec_balance(chars: List[Char], counting: Int, waiting: Boolean): Boolean = {
            if (chars.isEmpty) 
                counting == 0 && !waiting
            else if (chars.head == '(')
                rec_balance(chars.tail, counting + 1, true)
            else if (chars.head == ')')
                rec_balance(chars.tail, counting - 1, false)
            else
                rec_balance(chars.tail, counting, waiting)
        }
        rec_balance(chars, 0, false)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (money == 0) 
            1
        else if (money < 0 || coins.isEmpty)
            0
        else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
