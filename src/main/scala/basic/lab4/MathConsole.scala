package basic.lab4
import cui._
class MathConsole extends Console {
  def execute(cmmd:String):String = {
    val tokens = cmmd.split("\\s+")
    try {
      tokens(0) match {
        case "add" =>
          var sum = 0d
          for (i <- 1 until tokens.length)
            sum += tokens(i).toDouble
          sum.toString
        case "mul" =>
          var prod = 1d
          for (i <- 1 until tokens.length)
            prod *= tokens(i).toDouble
          prod.toString
        case "sub" =>
          var diff = tokens(1).toDouble
          for (i <- 2 until tokens.length)
            diff -= tokens(i).toDouble
          diff.toString
        case "div" =>
          var quot = tokens(1).toDouble
          for (i <- 2 until tokens.length) {
            if(tokens(i).toDouble==0d)
              throw new UserError("No division by 0")
            quot /= tokens(i).toDouble
          }
          quot.toString
        case _ =>
          throw new UserError("Unrecognized operator: " + tokens(0))
      }
    }
    catch{
      case e:NumberFormatException => throw new UserError("Arguments must be numbers")
      case e:ArrayIndexOutOfBoundsException => throw new UserError("Provide at least 1 argument")
      case e:UserError => throw e
    }
  }
}

