package lab5

trait Instruction{
  def execute:Unit
}
object Accumulator{
  var program:List[Instruction] = null
  var register:Double = 0
  var ip:Int = 0;
  def run():Unit={
    register=0
    ip=0
    while (ip>= 0 && ip<program.size){
      program(ip).execute
      ip+=1
    }
  }
}
class Add(val arg:Double) extends Instruction{
  override def execute:Unit = {
    Accumulator.register+=arg
  }
}
class Mul(val arg:Double) extends Instruction{
  override def execute:Unit = {
    Accumulator.register*=arg
  }
}
class Rep(val num:Int, val inst:Instruction) extends Instruction{
  override def execute: Unit = {
    for (i<-0 until num)
        inst.execute
  }
}
class Halt() extends Instruction{
  override def execute: Unit = {
    Accumulator.ip = -2
  }
}
class Blt(val limit:Double, val steps:Int) extends Instruction {
  override def execute: Unit = {
    if (Accumulator.register<limit)
      Accumulator.ip+=steps-1
  }
}
object testAccumulator extends App {
  // computing 3 * 4 + 9
  Accumulator.program = List(Add(3), Mul(4), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing (((10 * 2) + 3) * 5)
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  println("register = " + Accumulator.register)


  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 42

  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Blt(33, 2), Add(10), Halt(), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 32
}