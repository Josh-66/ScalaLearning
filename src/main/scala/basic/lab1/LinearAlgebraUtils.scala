package basic.lab1

class LinearAlgebraUtils {

  def dim(mat: Array[Array[Double]]): (Int, Int) = {
    val rowLength:Int = mat(0).length
    for (row <- mat)
      if (row.length!=rowLength)
        throw new Exception("Rows have different length")
    (mat.length,rowLength)
  }


  def dot(vec1: Array[Double], vec2: Array[Double]): Double =
    if (vec1.length!=vec2.length)
        throw new Exception("Vectors have different lengths")
    else {
      var dotProd = 0d
      for (i <- vec1.indices)
        dotProd += vec1(i) * vec2(i)
      dotProd
    }

  def product(mat: Array[Array[Double]], vec: Array[Double]): Array[Double] =
    if (dim(mat)(1)!=vec.length)
      throw new Exception("Dimensions don't match")
    else {
      val prod: Array[Double] = new Array[Double](mat.length)
      for (i <- mat.indices)
        for (j <- vec.indices)
          prod(i) = prod(i) + mat(i)(j) * vec(j)
      prod
    }


  def transpose(mat: Array[Array[Double]]): Array[Array[Double]] = {
    try {
      val dimension:(Int,Int) = dim(mat)
      val trans:Array[Array[Double]] = new Array[Array[Double]](dimension(1))
      for (i <- 0 until dimension(1)) {
        trans(i) = new Array[Double](dimension(0))
        for (j <- 0 until dimension(0))
          trans(i)(j)=mat(j)(i)
      }
      trans
    }
    catch{
      case e:Exception=>throw e
    }
  }

  def sum(vec1: Array[Double], vec2: Array[Double]): Array[Double] = {
    if (vec1.length!=vec2.length)
      throw new Exception("These vectors cannot be added")
    val sum = new Array[Double](vec1.length)
    for (i <- vec1.indices)
      sum(i)=vec1(i)+vec2(i)
    sum
  }

  def sum(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]]  =
    try{
      if (dim(mat1)!=dim(mat2)){
        throw new Exception("These matrices cannot be summed")
      }
      else{
        val sum = new Array[Array[Double]](mat1.length)
        for (i <- sum.indices) {
          sum(i) = new Array[Double](mat1(i).length)
          for (j <- sum(i).indices)
            sum(i)(j)=mat1(i)(j)+mat2(i)(j)
        }
        sum
      }
    }
    catch{
      case e:Exception=>throw e
    }

  def trace(mat: Array[Array[Double]]):Double =
    try {
      val dimension = dim(mat)
      if (dimension(0)!=dimension(1))
        throw new Exception("Cannot trace a non-square matrix")
      var traceSum = 0d
      for (i <- mat.indices)
        traceSum+=mat(i)(i)
      traceSum
    }
    catch{
      case e:Exception=>throw e
    }

  def toString(vec: Array[Double]): String = {
    val str = "[" + vec.mkString(",") + "]"
    str
  }


  def toString(mat: Array[Array[Double]]): String = {
    var str = ""
    for (i <- mat)
      str +=toString(i)+"\n"
    str
  }
}

