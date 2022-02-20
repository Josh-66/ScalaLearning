package lab2

object Fibonacci extends App {

  // non-tail recursive version
  def fib(n: Int): Int =
    if (n < 0)
      throw Exception("No negative inputs")
    if (n <= 1)
      1
    else
      fib(n - 2) + fib(n - 1)


  // tail recursive version
  def tailFib(n: Int): Int =
    def helper(count: Int, prev1: Int, prev2: Int): Int =
      if (count < n - 1)
        helper(count + 1, prev2, prev1 + prev2)
      else
        prev2

    helper(0, 1, 1)

  // the first 11 Fibonaccis
  for (i <- 0 to 10)
    println(s"fib(${i}) = ${Fibonacci.fib(i)}")
    println(s"tailFib(${i}) = ${Fibonacci.tailFib(i)}")

}

/*
output:

fib(0) = 1
tailFib(0) = 1
fib(1) = 1
tailFib(1) = 2
fib(2) = 2
tailFib(2) = 3
fib(3) = 3
tailFib(3) = 5
fib(4) = 5
tailFib(4) = 8
fib(5) = 8
tailFib(5) = 13
fib(6) = 13
tailFib(6) = 21
fib(7) = 21
tailFib(7) = 34
fib(8) = 34
tailFib(8) = 55
fib(9) = 55
tailFib(9) = 89
fib(10) = 89
tailFib(10) = 144
*/