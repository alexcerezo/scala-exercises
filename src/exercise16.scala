import scala.annotation.tailrec

class exercise16 (val numerator: Int, val denominator: Int) {

  require(denominator != 0, "Denominator can't be zero")

  private val gcd: Int = computeGcd(numerator.abs, denominator.abs)

  private val num: Int = numerator / gcd
  private val den: Int = denominator / gcd

  def +(that: exercise16): exercise16 = {
    new exercise16(num * that.den + that.num * den, den * that.den)
  }

  def -(that: exercise16): exercise16 = {
    new exercise16(num * that.den - that.num * den, den * that.den)
  }

  def *(that: exercise16): exercise16 = {
    new exercise16(num * that.num, den * that.den)
  }

  def /(that: exercise16): exercise16 = {
    new exercise16(num * that.den, den * that.num)
  }

  @tailrec
  private def computeGcd(a: Int, b: Int): Int = {
    if (b == 0) a else computeGcd(b, a % b)
  }
}

// Uso de la clase
object FractionApp extends App {
  val fraction1 = new exercise16(1, 2)
  val fraction2 = new exercise16(1, 3)
  val fraction3 = new exercise16(1, 4)
  val fraction4 = new exercise16(1, 5)

  println(s"Sum: ${fraction1 + fraction2}")
  println(s"Subtraction: ${fraction1 - fraction2}")
  println(s"Multiplication: ${fraction1 * fraction2}")
  println(s"Division: ${fraction1 / fraction2}")
}
