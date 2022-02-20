package lab2

import scala.collection.mutable

class Hanoi(rings: Int = 3) {
  var numRings = rings
  val numTowers = 3
  val towers: Array[mutable.Stack[Int]] = Array.ofDim[mutable.Stack[Int]](numTowers)

  // initialize towers
  for(t <- 0 until numTowers) towers(t) = mutable.Stack[Int]()
  // push ..., 3, 2, 1 onto tower 0
  for(r <- numRings to 1 by -1) towers(0).push(r) // towers = [Stack(1, 2, 3) Stack() Stack()]

  override def toString = {
    var result = "["
    for(t <- 0 until numTowers) {
      result += towers(t).toString + " "
    }
    result + "]"
  }

  // legally move num rings from fromTower to toTower
  def move(num: Int, fromTower: Int, toTower: Int): Unit = {
    if(num==1)
      println(s" $toString  Moving: ${towers(fromTower).top} from $fromTower to $toTower")
      val movedDisc = towers(fromTower).pop()
      if (towers(toTower).length==0 || movedDisc<towers(toTower).top)
        towers(toTower).push(movedDisc)
      else
        throw Exception(s"Larger disc being placed on top of smaller disc!")
    else
      val unused = 3-fromTower-toTower

      move(num-1,fromTower,unused)
      move(1,fromTower, toTower)
      move(num-1,unused,toTower)

  }

}



object Tower extends App {
  var game = Hanoi()
  println(game)
  // move 3 rings from tower 0 to tower 1
  game.move(3, 0, 2)
  println(game)

  println("--------------")

  game = Hanoi(6)
  println(game)
  // move 3 rings from tower 0 to tower 1
  game.move(6, 0, 2)
  println(game)



}
/*
Output:
[Stack(1, 2, 3) Stack() Stack() ]
 [Stack(1, 2, 3) Stack() Stack() ]  Moving: 1 from 0 to 2
 [Stack(2, 3) Stack() Stack(1) ]  Moving: 2 from 0 to 1
 [Stack(3) Stack(2) Stack(1) ]  Moving: 1 from 2 to 1
 [Stack(3) Stack(1, 2) Stack() ]  Moving: 3 from 0 to 2
 [Stack() Stack(1, 2) Stack(3) ]  Moving: 1 from 1 to 0
 [Stack(1) Stack(2) Stack(3) ]  Moving: 2 from 1 to 2
 [Stack(1) Stack() Stack(2, 3) ]  Moving: 1 from 0 to 2
[Stack() Stack() Stack(1, 2, 3) ]
--------------
[Stack(1, 2, 3, 4, 5, 6) Stack() Stack() ]
 [Stack(1, 2, 3, 4, 5, 6) Stack() Stack() ]  Moving: 1 from 0 to 1
 [Stack(2, 3, 4, 5, 6) Stack(1) Stack() ]  Moving: 2 from 0 to 2
 [Stack(3, 4, 5, 6) Stack(1) Stack(2) ]  Moving: 1 from 1 to 2
 [Stack(3, 4, 5, 6) Stack() Stack(1, 2) ]  Moving: 3 from 0 to 1
 [Stack(4, 5, 6) Stack(3) Stack(1, 2) ]  Moving: 1 from 2 to 0
 [Stack(1, 4, 5, 6) Stack(3) Stack(2) ]  Moving: 2 from 2 to 1
 [Stack(1, 4, 5, 6) Stack(2, 3) Stack() ]  Moving: 1 from 0 to 1
 [Stack(4, 5, 6) Stack(1, 2, 3) Stack() ]  Moving: 4 from 0 to 2
 [Stack(5, 6) Stack(1, 2, 3) Stack(4) ]  Moving: 1 from 1 to 2
 [Stack(5, 6) Stack(2, 3) Stack(1, 4) ]  Moving: 2 from 1 to 0
 [Stack(2, 5, 6) Stack(3) Stack(1, 4) ]  Moving: 1 from 2 to 0
 [Stack(1, 2, 5, 6) Stack(3) Stack(4) ]  Moving: 3 from 1 to 2
 [Stack(1, 2, 5, 6) Stack() Stack(3, 4) ]  Moving: 1 from 0 to 1
 [Stack(2, 5, 6) Stack(1) Stack(3, 4) ]  Moving: 2 from 0 to 2
 [Stack(5, 6) Stack(1) Stack(2, 3, 4) ]  Moving: 1 from 1 to 2
 [Stack(5, 6) Stack() Stack(1, 2, 3, 4) ]  Moving: 5 from 0 to 1
 [Stack(6) Stack(5) Stack(1, 2, 3, 4) ]  Moving: 1 from 2 to 0
 [Stack(1, 6) Stack(5) Stack(2, 3, 4) ]  Moving: 2 from 2 to 1
 [Stack(1, 6) Stack(2, 5) Stack(3, 4) ]  Moving: 1 from 0 to 1
 [Stack(6) Stack(1, 2, 5) Stack(3, 4) ]  Moving: 3 from 2 to 0
 [Stack(3, 6) Stack(1, 2, 5) Stack(4) ]  Moving: 1 from 1 to 2
 [Stack(3, 6) Stack(2, 5) Stack(1, 4) ]  Moving: 2 from 1 to 0
 [Stack(2, 3, 6) Stack(5) Stack(1, 4) ]  Moving: 1 from 2 to 0
 [Stack(1, 2, 3, 6) Stack(5) Stack(4) ]  Moving: 4 from 2 to 1
 [Stack(1, 2, 3, 6) Stack(4, 5) Stack() ]  Moving: 1 from 0 to 1
 [Stack(2, 3, 6) Stack(1, 4, 5) Stack() ]  Moving: 2 from 0 to 2
 [Stack(3, 6) Stack(1, 4, 5) Stack(2) ]  Moving: 1 from 1 to 2
 [Stack(3, 6) Stack(4, 5) Stack(1, 2) ]  Moving: 3 from 0 to 1
 [Stack(6) Stack(3, 4, 5) Stack(1, 2) ]  Moving: 1 from 2 to 0
 [Stack(1, 6) Stack(3, 4, 5) Stack(2) ]  Moving: 2 from 2 to 1
 [Stack(1, 6) Stack(2, 3, 4, 5) Stack() ]  Moving: 1 from 0 to 1
 [Stack(6) Stack(1, 2, 3, 4, 5) Stack() ]  Moving: 6 from 0 to 2
 [Stack() Stack(1, 2, 3, 4, 5) Stack(6) ]  Moving: 1 from 1 to 2
 [Stack() Stack(2, 3, 4, 5) Stack(1, 6) ]  Moving: 2 from 1 to 0
 [Stack(2) Stack(3, 4, 5) Stack(1, 6) ]  Moving: 1 from 2 to 0
 [Stack(1, 2) Stack(3, 4, 5) Stack(6) ]  Moving: 3 from 1 to 2
 [Stack(1, 2) Stack(4, 5) Stack(3, 6) ]  Moving: 1 from 0 to 1
 [Stack(2) Stack(1, 4, 5) Stack(3, 6) ]  Moving: 2 from 0 to 2
 [Stack() Stack(1, 4, 5) Stack(2, 3, 6) ]  Moving: 1 from 1 to 2
 [Stack() Stack(4, 5) Stack(1, 2, 3, 6) ]  Moving: 4 from 1 to 0
 [Stack(4) Stack(5) Stack(1, 2, 3, 6) ]  Moving: 1 from 2 to 0
 [Stack(1, 4) Stack(5) Stack(2, 3, 6) ]  Moving: 2 from 2 to 1
 [Stack(1, 4) Stack(2, 5) Stack(3, 6) ]  Moving: 1 from 0 to 1
 [Stack(4) Stack(1, 2, 5) Stack(3, 6) ]  Moving: 3 from 2 to 0
 [Stack(3, 4) Stack(1, 2, 5) Stack(6) ]  Moving: 1 from 1 to 2
 [Stack(3, 4) Stack(2, 5) Stack(1, 6) ]  Moving: 2 from 1 to 0
 [Stack(2, 3, 4) Stack(5) Stack(1, 6) ]  Moving: 1 from 2 to 0
 [Stack(1, 2, 3, 4) Stack(5) Stack(6) ]  Moving: 5 from 1 to 2
 [Stack(1, 2, 3, 4) Stack() Stack(5, 6) ]  Moving: 1 from 0 to 1
 [Stack(2, 3, 4) Stack(1) Stack(5, 6) ]  Moving: 2 from 0 to 2
 [Stack(3, 4) Stack(1) Stack(2, 5, 6) ]  Moving: 1 from 1 to 2
 [Stack(3, 4) Stack() Stack(1, 2, 5, 6) ]  Moving: 3 from 0 to 1
 [Stack(4) Stack(3) Stack(1, 2, 5, 6) ]  Moving: 1 from 2 to 0
 [Stack(1, 4) Stack(3) Stack(2, 5, 6) ]  Moving: 2 from 2 to 1
 [Stack(1, 4) Stack(2, 3) Stack(5, 6) ]  Moving: 1 from 0 to 1
 [Stack(4) Stack(1, 2, 3) Stack(5, 6) ]  Moving: 4 from 0 to 2
 [Stack() Stack(1, 2, 3) Stack(4, 5, 6) ]  Moving: 1 from 1 to 2
 [Stack() Stack(2, 3) Stack(1, 4, 5, 6) ]  Moving: 2 from 1 to 0
 [Stack(2) Stack(3) Stack(1, 4, 5, 6) ]  Moving: 1 from 2 to 0
 [Stack(1, 2) Stack(3) Stack(4, 5, 6) ]  Moving: 3 from 1 to 2
 [Stack(1, 2) Stack() Stack(3, 4, 5, 6) ]  Moving: 1 from 0 to 1
 [Stack(2) Stack(1) Stack(3, 4, 5, 6) ]  Moving: 2 from 0 to 2
 [Stack() Stack(1) Stack(2, 3, 4, 5, 6) ]  Moving: 1 from 1 to 2
[Stack() Stack() Stack(1, 2, 3, 4, 5, 6) ]

Process finished with exit code 0

*/