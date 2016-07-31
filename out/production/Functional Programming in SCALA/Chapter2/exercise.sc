object exercise {

  /**
    *  Exercise 2.1
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, now: Int): Int = {
      if (n <= 0) now
      else loop(n - 1, now, prev + now)
    }
    loop(n, 0, 1)
  }

  fib(0)
  fib(1)
  fib(2)
  fib(3)
  fib(4)
  fib(5)
}