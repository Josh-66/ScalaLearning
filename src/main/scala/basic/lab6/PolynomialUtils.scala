package basic.lab6

import scala.collection.mutable.ListBuffer

class PolynomialUtils {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] = {
    var disc = p(1)*p(1)-4*p(0)*p(2)
    if (disc<0)
        None
    else {
      disc = Math.sqrt(disc)
      Some((-p(1)+disc)/(2*p(0)),(-p(1)-disc)/(2*p(0)))
    }

  }

  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
    (0,p(0)*2,p(1))

  def eval(a: Double, p: (Double, Double, Double)): Double =
    a*a*p(0)+a*p(1)+p(2)

  def toString(p: (Double, Double, Double)):String = {
    val terms = ListBuffer[String]()
    if (p(0) != 0d)
      terms += (p(0) + "x^2")
    if (p(1) != 0d)
      terms += (p(1) + "x")
    if (p(2) != 0d)
      terms += p(2).toString
    terms.mkString(" + ")
  }
}




