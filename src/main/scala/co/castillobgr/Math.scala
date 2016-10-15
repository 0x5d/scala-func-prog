package co.castillobgr

object Math {
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, acc: Int): Int = {
      if (n < 1) acc
      else loop(n - 1, acc, prev + acc)
    }
    loop(n, 0, 1)
  }

  def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int): Int = {
      if (n < 1) acc
      else loop(n - 1, n * acc)
    }
    loop(n, 1)
  }
}
