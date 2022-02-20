package basic.lab2

object numerology extends App {

  // problem 1
  def kingdom(n: Int): Int =
    if (n % 2 == 0)
      if(n > 10)
        if (n % 100 == 0) 2
        else 1
      else 3
    else
      if(n>10) 4
      else 3



  //  // problem 2
  //
  //  def order(n: Int): Int = ???
  //
  //  def family(n: Int) = ???
  //  def ilk(n: Int) = ???
  //  def genus(n: Int) = ???
  //
  //  println(s"order(-1) = ${order(-1)}")   // -1
  //  println(s"order(15) = ${order(15)}")   // 10
  //  println(s"order(50) = ${order(50)}")   // 12
  //  println(s"order(49) = ${order(49)}")   // 13
  //  println(s"order(21) = ${order(21)}")   // 9

  // problem 3

  // original version
  // this function needs two elses
  def species(n: Int) =
    if (0 < n) if (n % 2 == 0) 1 else 2

  // corrected version
  def species2(n: Int): Int =
    if (n > 0)
      if (n % 2 == 0) 1
      else 2
    else 2



  // problem 4

  // odd positives are realm 1
  def realm1(n: Int): Int =
    if (n > 0)
      if (n % 2 == 1) 1
      else throw Exception()
    else throw Exception()


  // even positives not divisible by 3 are realm 2
  def realm2(n: Int): Int =
    if (n > 0)
      if (n % 2 == 0)
        if(n % 3 != 0) 2
        else throw Exception()
      else throw Exception()
    else throw Exception()

  // even positives divisible by 6 and 7 are realm 3
  def realm3(n: Int): Int =
    if (n > 0)
      if (n % 6 == 0)
        if (n % 7 == 0) 3
        else throw Exception()
      else throw Exception()
    else throw Exception()

  def realm(n: Int): Int = {
    try
      return realm1(n)
    catch {
      case e: Exception =>
    }
    try
      return realm2(n)
    catch {
      case e: Exception =>
    }
    try
      return realm3(n)
    catch {
      case e: Exception =>
    }
    0
  }

  // problem 6

  // odd positives are realm 1
  def realm1Opt(n: Int): Option[Int] =
    if (n > 0)
      if (n % 2 == 1) Some(1)
      else None
    else None

  // even positives not divisible by 3 are realm 2
  // another style: check for bad news first
  def realm2Opt(n: Int): Option[Int] =
    if (n > 0)
      if (n % 2 == 0)
        if(n % 3 != 0) Some(2)
        else None
      else None
    else None

  // even positives divisible by 6 and 7 are realm 3
  def realm3Opt(n: Int): Option[Int] =
    if (n > 0)
      if (n % 6 == 0)
        if (n % 7 == 0) Some(3)
        else None
      else None
    else None

  def realmOpt(n: Int):Int  = {
    realm1Opt(n) match {
      case Some(x) => x
      case None =>
    }
    realm2Opt(n) match {
      case Some(x) => return x
      case None =>
    }
    realm3Opt(n) match {
      case Some(x) => return x
      case None =>
    }
    0
  }

}
