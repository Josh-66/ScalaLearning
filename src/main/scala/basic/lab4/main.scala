package  basic.lab4

object main {
  def main(args: Array[String]): Unit = {
    val cui = new MathConsole()
    test()
    cui.run(args)
  }

  def test():Unit = {
    val cui = new MathConsole()
    cui.run(Array(
      "add 2 3 4 5 6",
      "mul 4 4 4 4",
      "sub",
      "sub 10",
      "sub 10 3 2 1",
      "div 5 3 .5",
      "div 6 2 x",
      "div 20 5 0",
      "zip 2 3 4"))
  }

}
/*
output:
->add 2 3 4 5 6
20.0
->mul 4 4 4 4
256.0
->sub
Provide at least 1 argument
->sub 10
10.0
->sub 10 3 2 1
4.0
->div 5 3 .5
3.3333333333333335
->div 6 2 x
Arguments must be numbers
->div 20 5 0
No division by 0
->zip 2 3 4
Unrecognized operator: zip
->zip
Unrecognized operator: zip
->
 */