package lab3

class DDS {
  final def controlLoop[S](state: S, cycle:Int, halt:(S,Int)=>Boolean,update:(S,Int)=>S):S=
    if (halt(state,cycle)) state
    else controlLoop(update(state,cycle),cycle+1,halt,update)

  val population = {
    controlLoop(1,0,
      (state:Int,cycle:Int)=>{state>1e5},
      (state:Int,cycle:Int)=>{state*2}
    )
  }

  def solve(f:Double=>Double): Double =
    val delta = 1e-7
    def goodEnuf(guess:Double,cycle:Int) = math.abs(f(guess))<delta
    def df(x:Double) = (f(x+delta)-f(x))/delta
    def improve(guess:Double,cycle:Int) = guess - (f(guess)/df(guess))
    controlLoop[Double](1.0,0,goodEnuf,improve)

  def sqrt(x:Double) = solve((n:Double)=> n*n-x)

  def cubeRoot(x:Double) = solve((n:Double)=>n * n * n - x)

  def nthRoot(x:Double, n:Int): Double ={
    def power(b:Double,e:Int,total:Double):Double ={
      if (e==0)
        total
      else
        power(b,e-1,total*b)
    }

    solve((n2:Double) => power(n2,n,1)-x)
  }

  // = value of an investment of $principle at an annual rate r compounded
  // periods times over 1 year
  def value(principle: Double, rate: Double, periods: Int): Double = {
    controlLoop(principle,1,
      (state:Double,cycle:Int)=>{cycle>periods},
      (state:Double,cycle:Int)=>{state * (1+rate/periods)}
    )
  }
}

object TestDDS extends DDS with App{

  println("Population:")
  println(population)

  println("Solve:")
  println("2x-4 = " + solve( (x:Double)=>{x*2-4})) // 1.9999... ~ 2
  println("sqrt(2) = " + sqrt(2)) // 1.4142...
  println("sqrt(100) = " + sqrt(100)) //10.000...

  println("cubeRoot(2) = "+cubeRoot(2) ) //1.2599...
  println("cubeRoot(27)= "+cubeRoot(27)) //3.0000...

  println("3rd root of 64 = " + nthRoot(64,3)) //4.0000...
  println("5th root of 371293 = " + nthRoot(371293,5)) //13.0000....

  println("$1 compounded at 100% monthly: " + value(1,1,12)) //2.613...
  println("$1 compounded at 100% daily: " + value(1,1,365)) //2.714...
  println("$1 compounded at 100% a lot: " + value(1,1,1_000_000)) //2.71828... ~ e


}

/*Output
Population:
131072
Solve:
2x-4 = 1.9999999994161328
sqrt(2) = 1.4142135623747674
sqrt(100) = 10.000000000139973
cubeRoot(2) = 1.2599210500187663
cubeRoot(27)= 3.0000000000001155
3rd root of 64 = 4.0000000000766365
5th root of 371293 = 13.000000000000293
$1 compounded at 100% monthly: 2.6130352902246763
$1 compounded at 100% daily: 2.7145674820219714
$1 compounded at 100% a lot: 2.7182804690959363
*/