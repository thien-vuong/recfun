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
      if (c == 0 || c == r) 1
      else if (c < 0 || c > r) throw new IllegalArgumentException
      else pascal(c - 1, r - 1) + pascal(c, r -1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(chars: List[Char], count: Int): Boolean = {
        if (count < 0) {
          return false
        }

        if (chars.isEmpty) {
          return (count == 0)
        }

        if (chars.head == ')') {
          balanceIter(chars.tail, count - 1)
        }
        else if (chars.head == '(') {
          balanceIter(chars.tail, count + 1)
        }
        else balanceIter(chars.tail, count)
      }

      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def reduce(money: Int, coins: List[Int], counter: Int): Int = {
        if (money == 0) counter + 1
        else if (money < 0 || coins.isEmpty) counter
        else reduce(money - coins.head, coins, counter) + reduce(money, coins.tail, counter)
      }

      if (money <= 0 || coins.isEmpty) 0
      else reduce(money, coins, 0)
    }
  }
