package lab5

package lab5
import cui._

class OutOfGas extends UserError("You are out of gas")
class Escaped extends UserError("You have escaped!")

enum Heading ???

class Robot ???

object maze extends Console with App {
  val rng = util.Random()
  var exit = (rng.nextInt(10), rng.nextInt(10))
  val robot = Robot("Robbie")

  def distance (p1: (Int, Int), p2: (Int, Int)) =
    val (a, b) = p1
    val (c, d) = p2
    (math.sqrt((a - c) * (a - c) + (b - d) * (b - d))).toInt

  instruction = "Commands ::= restart | move STEPS | turn HEADING"

  override def execute(cmmd: String) = ???

  repl // start the repl

}

