package fpinscala.gettingstarted

object Exercises {

  def fib(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else {
      @annotation.tailrec
      def go(cnt: Int, x1: Int, x2: Int): Int = {
        if (cnt >= n) x1 + x2
        else {
          go(cnt + 1, x2, x1 + x2)
        }
      }
      go(3, 0, 1)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    val n = 6
    println("The first " + n + " fibonacci numbers are: ")
    for (i <- 1 to n)
      print(fib(i))
    println(" ")

    val sortedNumbers = Array(1, 2, 3, 5, 7, 9)
    val unsortedNumbers = Array(1, 2, 3, 6, 9, 0)

    def smaller(a: Int, b: Int): Boolean = a < b

    println("The sorted array is sorted: " + isSorted(sortedNumbers, smaller))
    println("The unsorted array is sorted: " + isSorted(unsortedNumbers, smaller))

  }
}
