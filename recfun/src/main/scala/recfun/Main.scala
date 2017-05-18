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
    def pascal(c: Int, r: Int): Int = {
      if ((c == 0) || (c == r)) 1
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance_helper(bh_chars : List[Char], open_count : Int) : Int = {
        if (bh_chars.isEmpty || (open_count < 0)) open_count
        else if (bh_chars.head == '(') balance_helper(bh_chars.tail, open_count + 1)
        else if (bh_chars.head == ')') balance_helper(bh_chars.tail, open_count - 1)
        else balance_helper(bh_chars.tail, open_count)
      }

      if (balance_helper(chars, 0) == 0) true else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if ((money < 0) || coins.isEmpty) 0
      else if ((money <= 0) && (!coins.isEmpty)) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
