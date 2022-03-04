package lab4

object LazyLists extends App {
  def infinite1s(): LazyList[Int]={
    1 #:: infinite1s()
  }
  def integers(): LazyList[Int]={
    def helper(num:Int): LazyList[Int] = {
      num #:: helper(num+1)
    }
    helper(0)
  }
  def evenIntegers()={
    integers().filter(_%2==0)
  }
  def squareIntegers()={
    integers().map(x=>x*x)
  }
  def print5(list: LazyList[Int]): Unit ={
    for (i <- 0 until 5)
      println(list(i))
  }

  println("Infinite1s:")
  print5(infinite1s())
  println("integers:")
  print5(integers())
  println("evenIntegers:")
  print5(evenIntegers())
  println("squareIntegers:")
  print5(squareIntegers())
}
