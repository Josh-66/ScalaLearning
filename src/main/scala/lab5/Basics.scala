package lab5
// ++++++++++++++++++++++++
// Implementing a reference class
// ++++++++++++++++++++++++

// given
enum LetterGrade:
  case A, B, C, D, F

class Assignment(val name:String, val id:Int,private var _grade:Int) {
  if (_grade <0 || _grade>100)
    throw Exception("Invalid Grade")

  def grade:Int = _grade

  def grade_=(amt:Int):Unit = {
    if (amt <0 || amt>100)
      throw Exception("Invalid Grade")
    _grade=amt
  }
  def letterGrade:LetterGrade = {
    if (grade>=90)
      LetterGrade.A
    else if (grade>=80)
      LetterGrade.B
    else if (grade>=70)
      LetterGrade.C
    else if (grade>=60)
      LetterGrade.D
    else
      LetterGrade.F

  }

  override def toString: String = s"${name} assn ${id}: ${grade} (=${letterGrade})"
}


object testAssignment extends App:

  try
    val simpson = Assignment("Simpson", 1, -88)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade

  val jones = Assignment("Jones", 1, 88)
  val hanson = Assignment("Hanson", 1, 95)

  println(jones.grade) // 88
  println(jones.letterGrade) // B
  jones.grade = jones.grade + 10
  println(jones.grade) //98
  println(jones.letterGrade) // A
  println(jones) // Jones assn 1: 98 (= A)

  println(hanson.grade) // 95
  println(hanson.letterGrade) // A
  try
    hanson.grade = hanson.grade + 10
  catch
    case e: Exception => println(e.getMessage) // Invalid grade
  finally
    println(hanson) // Hanson assn 1: 95 (= A)
  try
    val smith = Assignment("Smith", 1, -10)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade


// ++++++++++++++++++++++++
// Implementing static variables & methods
// ++++++++++++++++++++++++

class Transaction(val from:Int, val to:Int, val amount:Double)
{

  if (amount<=0)
    throw new Exception("Invalid Amount")
  private val id = Transaction.nextId

  override def toString: String = "Transaction #"+id+": $"+amount+" from acct "+from+ " to acct " +to


}
object Transaction{
  var _id = 499
  def nextId = {
    _id+=1
    _id
  }

}
object testTransactions extends App:
  try
    val t1 = Transaction(119, 212, -20.50)
  catch
    case e: Exception => println(e.getMessage) // Invalid amount
  val ledger = List(
    Transaction(119, 212, 600.50),
    Transaction(212, 119, 1200),
    Transaction(212, 119, 98.75)
  )
  ledger.foreach(println) // how to create unique IDs
/*
Transaction #500: $600.5 from acct 119 to acct 212
Transaction #501: $1200.0 from acct 212 to acct 119
Transaction #502: $98.75 from acct 212 to acct 119
*/

// ++++++++++++++++++++++++
// Implementing a value class
// ++++++++++++++++++++++++

class Time(val hour:Int, val min:Int = 0) extends Ordered[Time]{
  def this (time:String)={
    this(time.split(":")(0).toInt,time.split(":")(1).toInt)

  }
  if (hour<0 || hour >=24)
    throw Exception("Invalid hour")
  if (min<0 || min >=60)
    throw Exception("Invalid minute")
  def +(other:Time): Time = {
    var newHour = hour + other.hour
    var newMinute = min + other.min
    if (newMinute >= 60) {
      newMinute -= 60
      newHour +=1
    }
    newHour = newHour % 24
    Time(newHour,newMinute)


  }
  override def equals(other:Any):Boolean={
    other match {
      case o:Time => o.isInstanceOf[Time] &&
                     o.hour==hour &&
                     o.min==min
      case _ => false
    }
  }
  override def toString: String =
    var str = hour+":"
    if (min<10)
      str+="0"
    str+=min
    str
  override def hashCode(): Int = toString.hashCode
  def compare(that:Time): Int = {
    if (hour > that.hour)
      1
    else if (hour<that.hour)
      -1
    else
      min-that.min
  }
}
class PreciseTime(hour:Int,min:Int = 0, val sec:Int = 0) extends Time(hour, min){

  if sec>=60 then throw Exception("Invalid Second")

  override def toString: String = {
    var str = super.toString+":"
    if (sec<10)
      str+="0"
    str+=sec
    str
  }
  override def equals(other:Any):Boolean={
    other match {
      case o:PreciseTime => o.isInstanceOf[PreciseTime] &&
        o.hour==hour &&
        o.min==min &&
        o.sec==sec
      case _ => false
    }
  }
}


object testTime extends App:
  try
    val t = Time(24, 50)
  catch
    case e: Exception => println(e.getMessage) // Invalid hour
  try
    val t = Time(12, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid minute

  val t1 = Time(10, 30)
  val t2 = Time(15, 45)
  val t3 = Time(10, 30)
  val t4 = Time("18:45")
  val t5 = Time(17)
  val t6 = t1 + t2
  println(t1) // 10:30
  println(t2) // 15:45
  println(t3) // 10:30
  println(t4) // 18:45
  println(t5) // 17:00
  println(t6) // 2:15
  println(t1 == t3) // true
  println(t1 != t5) // true
  println(t1 < t2)  // true
  println(t4 < t2)  // false
  println(t1 <= t3) // true

  val schedule = Map(
    t1 -> "coffee break",
    t2 -> "nap",
    t4 -> "cocktail hour"
  )
  println(schedule) // Map(10:30 -> coffee break, 15:45 -> nap, 18:45 -> cocktail hour)
  println(schedule(t3)) // coffee break

  try
    val pt = PreciseTime(12, 0, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid second

  val pt2 = PreciseTime(18)
  val pt1 = PreciseTime(10, 30)
  println(pt1) // 10:30:00
  println(pt2) // 18:00:00
  println(t1) // 10:30
  println(pt1 == t1) // false
  try
    println(schedule(pt1))
  catch
    case e: Exception => println(e.getMessage) // key not found: 10:30:00

