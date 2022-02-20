package basic.lab2

import basic.lab2.numerology._

object main extends App {
  // problem 1
  println(s"kingdom(11) = ${kingdom(11)}") // 4
  println(s"kingdom(16) = ${kingdom(16)}") // 1
  println(s"kingdom(5) = ${kingdom(5)}") // 3
  println(s"kingdom(200) = ${kingdom(200)}") // 2

  //problem 3
  println(s"species2(0) = ${species2(0)}") // 2
  println(s"species2(4) = ${species2(4)}") // 1
  println(s"species2(5) = ${species2(5)}") // 2
  println(s"species2(-2) = ${species2(-2)}") //2

  // problem 4
  println(s"realm(0) = ${realm(0)}") // 0
  println(s"realm(5) = ${realm(4)}") // 2
  println(s"realm(42) = ${realm(42)}") // 3
  println(s"realm(9) = ${realm(9)}") // 1

  //problem 5
  println(s"realmOpt(0) = ${realmOpt(0)}") // 0
  println(s"realmOpt(5) = ${realmOpt(4)}") // 2
  println(s"realmOpt(42) = ${realmOpt(42)}") // 3
  println(s"realmOpt(9) = ${realmOpt(9)}") // 1
}
/*
output:
kingdom(11) = 4
kingdom(16) = 1
kingdom(5) = 3
kingdom(200) = 2
species2(0) = 2
species2(4) = 1
species2(5) = 2
species2(-2) = 2
realm(0) = 0
realm(5) = 2
realm(42) = 3
realm(9) = 1
realmOpt(0) = 0
realmOpt(5) = 2
realmOpt(42) = 3
realmOpt(9) = 1

Process finished with exit code 0

 */