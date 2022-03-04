package lab4

class Transaction(val amt: Double, val fromAcct: Int, val toAcct: Int)

def balance(acct: Int, ledger: List[Transaction]): Double = {
  ledger.map(x => if x.fromAcct==acct then -x.amt else if x.toAcct==acct then x.amt else 0).reduce(_ + _)
}


object LedgerTest extends App{
  val ledger = List(
    new Transaction(12,0,2),
  new Transaction(116,1,0),
  new Transaction(632,2,1),
  new Transaction(32,0,2),
  new Transaction(22,1,0),
  )
  println(balance(0,ledger))
}