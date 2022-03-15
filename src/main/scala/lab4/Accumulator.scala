package lab4

def accum(program: List[Int=>Int]): Int = {
  def execute(result:Int,remaining: List[Int=>Int]):Int={
    if (remaining.isEmpty)
      result
    else
      execute(remaining.head(result),remaining.tail)
  }
  execute(0,program)
}

def add(num:Int) = (x:Int)=>x+num
def mul(num:Int) = (x:Int)=>x*num
def display(x:Int) ={
  println(s"register = ${x}")
  x
}
def clear(x:Int) = 0

object AccumTest extends App{
  val program =
    List(add(3), mul(4), add(5), display _, clear _, add(9), mul(2))

  println(accum(program))

}