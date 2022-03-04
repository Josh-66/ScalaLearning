package lab3

class Lab3 {

  def compose[A,B,C](f:(B) => C , g:(A)=>B) = {
    (x:A)=> f(g(x))
  }



  def id[T](x: T) = x
  def selfIter[T](f: T => T, n: Int): T => T = {
    def helper(g:T => T, n: Int): T => T ={
      if (n == 0)
        id
      else if (n==1)
        g
      else
        helper(compose(f,g),n-1)
    }

    helper(f,n)
  }


  def countPass[T](elems: Array[T], test: T => Boolean): Int = {
    def helper(index:Int, count:Int):Int = {
      if (index>=elems.length)
        count
      else if test(elems(index)) then
        helper(index+1,count+1)
      else
        helper(index+1,count)
    }
//    var count = 0
//    for (i <- elems){
//      if (test(i))
//        count = count+1
//    }
//    count
    helper(0,0)
  }


  def makeIter(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
    (x:Int) => {
      var result = baseVal;
      for (i <- 1 to x){
        result = combiner(result,i)
      }
      result
    }
  }


  def deOptionize[T, S](f: T => Option[S]): T => S = {
    (x:T)=>{
      f(x) match
        case Some(n) => n
        case None => throw Exception("Invalid input:" + x)
    }
  }

}