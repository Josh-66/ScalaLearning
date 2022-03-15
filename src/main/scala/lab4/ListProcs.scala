package lab4

import scala.annotation.tailrec

// pipeline implementations
object pipes {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = {
    elems.filter(_%2==1).map(x=>x*x*x).reduce(_ + _)
  }
  // sum of sums
  def sos(lists: List[List[Double]]): Double = {
    lists.map(_.reduce(_+_)).reduce(_+_)
  }
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = {
    vals.map(x=>if test(x) then 1 else 0).reduce(_+_)
  }
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = {
    vals.map(test).reduce(_ || _)
  }
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = {
    vals.map(test).reduce(_ && _)
  }
}

// iterative implementations
object iters {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = {
    var sum = 0
    for (i <- elems if i%2==1)
      sum+=i*i*i
    sum
  }
  // sum of sums
  def sos(lists: List[List[Double]]): Double = {
      var totalSum = 0d
      for (list <- lists)
        for (i <- list)
          totalSum+=i
      totalSum
  }
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = {
    var totalPass = 0
    for (i<-vals if test(i))
      totalPass+=1
    totalPass
  }
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = {
    for (i<-vals if test(i))
      return true
    false
  }
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = {
    for (i<-vals if !test(i))
      return false
    true
  }
}

// tail recursive implementations
object tails {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = {
    @tailrec
    def helper(result:Int, remaining:List[Int]):Int={
      if (remaining.isEmpty)
        result
      else if(remaining.head%2==1)
        helper(result+remaining.head*remaining.head*remaining.head,remaining.tail)
      else
        helper(result,remaining.tail)
    }
    helper(0,elems)
  }
  // sum of sums
  def sos(lists: List[List[Double]]): Double = {
    def helper1(result:Double,remaining:List[Double]): Double ={
      if (remaining.isEmpty)
        result
      else
        helper1(result+remaining.head,remaining.tail)
    }
    def helper2(result:Double,remaining:List[List[Double]]):Double ={
      if(remaining.isEmpty)
        result
      else
        helper2(result+helper1(0,remaining.head),remaining.tail)
    }
    helper2(0,lists)

  }
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = {
    @tailrec
    def helper(result:Int, remaining:List[T]):Int = {
      if (remaining.isEmpty)
        result
      else if (test(remaining.head))
        helper(result + 1, remaining.tail)
      else
        helper(result, remaining.tail)
    }
    helper(0,vals)

  }
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = {
    @tailrec
    def helper(result:Boolean, remaining: List[T]): Boolean ={
      if (remaining.isEmpty)
        result
      else
        helper(result||test(remaining.head),remaining.tail)
    }
    helper(false,vals)
  }
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = {
    @tailrec
    def helper(result:Boolean, remaining: List[T]): Boolean ={
      if (remaining.isEmpty)
        result
      else
        helper(result&&test(remaining.head),remaining.tail)
    }
    helper(true,vals)
  }
}

// classic recursive implementations (i.e., not tail recursive)
object recur {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = {
    if (elems.isEmpty)
      0
    else if (elems.head%2==1)
      elems.head*elems.head*elems.head + socs(elems.tail)
    else
      socs(elems.tail)

  }
  // sum of sums
  def sos(lists: List[List[Double]]): Double = {
    def sum(list:List[Double]):Double = {
      if (list.isEmpty)
        0
      else
        list.head+sum(list.tail)
    }
    if (lists.isEmpty)
      0
    else
      sum(lists.head)+sos(lists.tail)
  }
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = {
    if (vals.isEmpty)
      0
    else if (test(vals.head))
      1 + countPass(vals.tail,test)
    else
      countPass(vals.tail,test)

  }
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = {
    if (vals.isEmpty)
      false
    else if (test(vals.head))
      true
    else
      somePass(vals.tail,test)
  }
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = {
    if (vals.isEmpty)
      true
    else if (!test(vals.head))
      false
    else
      allPass(vals.tail,test)
  }
}

object ListProcs extends App {

  println("Testing pipelines")
  println("" + pipes.socs(List(1, 2, 3))) // 28
  println("" + pipes.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + pipes.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + pipes.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + pipes.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing iterations")
  println("" + iters.socs(List(1, 2, 3))) // 28
  println("" + iters.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + iters.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + iters.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + iters.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing tail-recursions")
  println("" + tails.socs(List(1, 2, 3))) // 28
  println("" + tails.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + tails.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + tails.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + tails.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing recursions")
  println("" + recur.socs(List(1, 2, 3))) // 28
  println("" + recur.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + recur.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + recur.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + recur.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

}
