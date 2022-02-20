package basic.lab6

object main extends PolynomialUtils with App{
  val poly = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)

  println("poly = " + toString(poly))
  println("eval(6, poly) = " + eval(6, poly))
  println("eval(2, poly) = " + eval(2, poly))
  println("eval(-5, poly) = " + eval(-5, poly))

  println("roots(poly) = " + roots(poly))

  println("deriv(poly) = " + toString(deriv(poly)))
  println("deriv2(poly) = " + toString(deriv(deriv(poly))))

}
/*
output
poly = 3.0x^2 + 9.0x + -30.0
eval(6, poly) = 132.0
eval(2, poly) = 0.0
eval(-5, poly) = 0.0
roots(poly) = Some((2.0,-5.0))
deriv(poly) = 6.0x + 9.0
deriv2(poly) = 6.0
*/
