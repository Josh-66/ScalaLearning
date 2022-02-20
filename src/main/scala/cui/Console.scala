package cui

import scala.io._

class UserError(gripe: String) extends Exception(gripe)

abstract class Console {
  var verbose = false // print stack traces if true

  // override in an extension
  def execute(cmmd: String): String


  def repl: Unit = {
    var more = true
    var text = ""
    while (more) {
      text = StdIn.readLine("->")
      if (text == "quit") {
        println("bye")
        more = false
      }
      else
        try
          println((execute(text)))
        catch {
          case e: UserError =>
            println(e.getMessage)
            if (verbose)
              e.printStackTrace()
        }

    }


  }
  def run(args:Array[String]) =
    if (args.isEmpty)
      repl
    else
      for(s <- args) {
        println(s"->${s}")
        try
          println((execute(s)))
        catch {
          case e: UserError =>println(e.getMessage)
        }
      }
}