package lab2

import sun.management.counter.Counter

import scala.annotation.tailrec

class Base {
  def inc(x: BigInt): BigInt = x + 1
  def dec(x: BigInt): BigInt = x - 1
  def isZero(x: BigInt): Boolean = x == 0
}

class Hyper extends Base {

  def add(n: BigInt, m: BigInt): BigInt = {
    if (!isZero(m))
      inc(add(n, dec(m)))
    else
      n

  }

  def mul(n: BigInt, m: BigInt): BigInt = {
    if(!isZero(dec(m)))
      add(n,mul(n,dec(m)))
    else
      n
  }

  def exp(n: BigInt): BigInt = {
    if(isZero(n))
      1
    else
      mul(2,exp(dec(n)))
  }

  def hyperExp(n: BigInt): BigInt = {
    if(isZero(n))
      2
    else
      exp(hyperExp(dec(n)))
  }

  // etc.
}

class TailHyper extends Base {

  final def add(n: BigInt, m: BigInt) = {
    def helper(count:BigInt, result:BigInt):BigInt =
      if (!isZero(count))
        helper (dec (count), inc (result) )
      else
        result
    helper(n,m)
  }

  def mul(n: BigInt, m: BigInt) = {
    def helper(count:BigInt, result:BigInt):BigInt =
      if (!isZero(count))
        helper (dec (count), add(result,m) )
      else
        result
    helper(n,0)
  }

  def exp(n: BigInt) = {
    def helper(count:BigInt, result:BigInt):BigInt =
      if (!isZero(count))
        helper (dec (count), mul(result,2) )
      else
        result
    helper(n,1)
  }

  def hyperExp(n: BigInt) = {
    def helper(count:BigInt, result:BigInt):BigInt =
      if (!isZero(count))
        helper (dec (count), exp(result) )
      else
        result
    helper(n,2)
  }

  // etc.
}

object main extends App {
  HyperTest()
  TailHyperTest()
}
object HyperTest extends Hyper{
  def apply() = {
    println("Hyper Test:")
    println("add(2,4) = " + add(5, 12)) // 17
    println("mul(2,6) = " + mul(3, 7)) // 21
    println("exp(10) = " + exp(10)) // 1024
    println("hyperExp(2) = " + hyperExp(2)) // 16
    try
      println("hyperExp(3) = " + hyperExp(3)) // stack overflow
    catch
      case e:StackOverflowError => println("hyperExp(3) = Stack overflow")
    try
      println("hyperExp(4) = " + hyperExp(4)) // stack overflow
    catch
      case e:StackOverflowError => println("hyperExp(3) = Stack overflow")

  }
}
object TailHyperTest extends TailHyper{

  def apply() = {
    println("TailHyper Test:")
    println("add(2,4) = " + add(5, 12)) // 17
    println("mul(2,6) = " + mul(3, 7)) // 21
    println("exp(10) = " + exp(10)) // 1024
    println("hyperExp(2) = " + hyperExp(2)) // 16
    println("hyperExp(3) = " + hyperExp(3)) // 65536
    println("hyperExp(4) = " + hyperExp(4))   // still waiting

  }
}

/*
Output:
Hyper Test:
add(2,4) = 17
mul(2,6) = 21
exp(10) = 1024
hyperExp(2) = 16
hyperExp(3) = Stack overflow
hyperExp(3) = Stack overflow
TailHyper Test:
add(2,4) = 17
mul(2,6) = 21
exp(10) = 1024
hyperExp(2) = 16
hyperExp(3) = 65536
*/