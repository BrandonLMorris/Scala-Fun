object Main {
  /** `def` does not evaluate the right-hand side upon assignment, but rather
    * evaluates it each use. `val`, on the other hand, does evaluate upon
    * assignment.
    */

  def main(args: Array[String]) = {
    println(circumference(7))

    // Doesn't terminate because `loop` tries to evaluate
    // println(first(1, loop))

    // Works because `loop` doesn't get evaluated
    println(firstByName(1, loop))

    println("sqrt(4): " + mySqrt(4))            // Should be about 2.0ish

    println("gcd(14, 21): " + gcd(14, 21))      // Should be 7

    println("5!: " + factorial(5))              // Should be 120
    println("5!: " + factTail(5))
  }

  def circumference(radius: Double): Double = {
    2 * Math.PI * radius
  }

  def square(x: Double): Double = x * x

  def sumOfSquares(x: Double, y: Double): Double = square(x) + square(y)

  // Infinite loop to demonstrate call-by-name and call-by-value
  def loop: Int = loop                        // `def` is very necessary

  def first(x: Int, y: Int) = x

  def firstByName(x: Int, y: => Int) = x     // Never attempts to evaluate y

  def abs(x: Double) = if (x >= 0) x else -x

  def mySqrt(x: Double) = {
    // Lexical scoping, woohoo!
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(square(guess) - x) < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

  // Tail recursive version of factorial
  def factTail(n: Int): Int = {
    def factorial(n: Int, acc: Int): Int =
      if (n == 0) acc
      else factorial(n-1, acc * n)

    factorial(n, 1)
  }
}
