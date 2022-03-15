package lab5

import cui.*
import jdk.jshell.spi.ExecutionControl.UserException

class OutOfGas extends UserError("You are out of gas")
class Escaped extends UserError("You have escaped!")

enum Heading {
  case north, east, south, west
}

class Robot(val name:String){
  var position:(Int,Int)=(0,0)
  var heading:Heading = Heading.east
  var fuel:Int = 100
  val escaped:Boolean = false
  def move(dist:Int): Unit = {
    heading match {
      case Heading.north=>position = (position(0) , position(1) + dist)
      case Heading.east=>position = (position(0)+ dist , position(1))
      case Heading.south=>position = (position(0) , position(1) - dist)
      case Heading.west=>position = (position(0)- dist , position(1) )

    }
  }
  def status = {
    s"Robbie at ${position} heading ${heading} with ${fuel} units of fuel. Distance to goal = ${maze.distance(position,maze.exit)}"
  }
}

object maze extends Console with App {
  val rng = util.Random()
  var exit = (rng.nextInt(10), rng.nextInt(10))
  val robot = Robot("Robbie")

  def distance (p1: (Int, Int), p2: (Int, Int)) =
    val (a, b) = p1
    val (c, d) = p2
    (math.sqrt((a - c) * (a - c) + (b - d) * (b - d))).toInt

  val instruction = "Commands ::= restart | move STEPS | turn HEADING"

  override def execute(cmmd: String) = {
    val cmdList:Array[String] = cmmd.split("\\s+")
    cmdList(0) match {
      case "move" =>
        try {
          robot.move(cmdList(1).toInt)

        } catch {
          case e: Exception => throw UserError("Steps must be a number")
        }
      case "turn" =>
        var newHead: Heading = null
        cmdList(1).toLowerCase() match {
          case "north" => newHead = Heading.north
          case "east" => newHead = Heading.east
          case "south" => newHead = Heading.south
          case "west" => newHead = Heading.west
          case _=>throw UserError("Invalid Heading")
        }
        robot.heading = newHead
    }
    robot.status
  }
  repl // start the repl

}

