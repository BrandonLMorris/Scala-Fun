/**
  * Exercise 2.1
  *
  * Write a recursive function to get the nth Fibonacci number, where the first
  * two Fibonacci numbers are defined to be 0 and 1.
  *
  * Created by Brandon Morris on 2/14/16.
  */

object Ex2_1 {

  /**
    * Find the n'th Fibonacci number, where fib(1) is 0, fib(2) is 1, and
    * fib(x) is fib(x-1) + fib(x-2)
    *
    * @param n the number in the Fibonacci sequence to find
    * @return the n'th number in the Fibonacci sequence
    */
  def fib(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else if (n > 2) fib(n-1) + fib(n-2)
    else
      throw new Exception("n must be positive")
  }

  /** Simple test runs */
  def main(args: Array[String]): Unit = {
    assert(0 == fib(1))
    assert(1 == fib(2))
    assert(1 == fib(3))
    assert(2 == fib(4))
    assert(3 == fib(5))
    assert(5 == fib(6))
    assert(8 == fib(7))
    assert(13 == fib(8))
    println("All tests passed")
  }
}
