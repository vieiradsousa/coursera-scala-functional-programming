package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
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
  def balance(chars: List[Char]): Boolean = isBalancedV2(chars, 0)

  def isBalancedV2(chars: List[Char], current: Int): Boolean = {
    if (chars.isEmpty) current == 0
    else if (chars.head == '(') isBalancedV2(chars.tail, current + 1)
    else if (chars.head == ')') current > 0 && isBalancedV2(chars.tail, current - 1)
    else isBalancedV2(chars.tail, current)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = count(money, coins)

  def count(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else count(money, coins.tail) + count(money - coins.head, coins)
  }
}
