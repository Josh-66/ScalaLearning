package basic.lab1

object main extends LinearAlgebraUtils with App {
  try {
    def vec1 = Array(5.0, 2.0, 6.0)

    def mat1 = Array(Array(1.0, 2.0, 3), Array(4.0, 5, 6), Array(7.0, 8, 9))

    def mat2 = Array(Array(10.0, 11, 12), Array(13.0, 14, 15), Array(16.0, 17, 18))

    println("vec1 = " + toString(vec1))
    println("mat1 = ")
    println(toString(mat1))
    println("mat2 = ")
    println(toString(mat2))
    println("dim(mat1) = " + dim(mat1))
    println("dot(mat1(0),mat1(1)) = " + dot(mat1(0),mat1(1)))
    println("sum(mat1(0),mat1(1)) = " + toString(sum(mat1(0),mat1(1))))
    println("trace(mat1) = " + trace(mat1))
    println("transpose(mat2) = ")
    println(toString(transpose(mat2)))
    println("sum(mat1, mat2) = ")
    println(toString(sum(mat1, mat2)))
    println("product(mat1, vec1) = " + toString(product(mat1, vec1)))
    val vec2 = Array(0.0, 4.0, 9.0, 8.0)
    println("product(mat1, vec1) = " + toString(product(mat1, vec2)))
  } catch {
    case e: Exception => println(e)
  }

}

/*
output:
vec1 = [5.0 2.0 6.0]
mat1 =
[1.0 2.0 3.0]
[4.0 5.0 6.0]
[7.0 8.0 9.0]

mat2 =
[10.0 11.0 12.0]
[13.0 14.0 15.0]
[16.0 17.0 18.0]

dim(mat1) = (3,3)
dot(mat1(0),mat1(1)) = 32.0
sum(mat1(0),mat1(1)) = [5.0,7.0,9.0]
trace(mat1) = 15.0
transpose(mat2) =
[10.0 13.0 16.0]
[11.0 14.0 17.0]
[12.0 15.0 18.0]

sum(mat1, mat2) =
[11.0 13.0 15.0]
[17.0 19.0 21.0]
[23.0 25.0 27.0]

product(mat1, vec1) = [27.0 66.0 105.0]
java.lang.Exception: Dimensions don't match
*/
