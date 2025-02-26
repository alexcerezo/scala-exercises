import scala.io.StdIn.readInt
object exercise4 {
  def isPrime(n: Int): Boolean = {
    if (n < 2) false
    else if (n == 2) true
    else !(2 until Math.sqrt(n).toInt + 1).exists(n % _ == 0)
  }

  def firstNPrimes(n: Int): List[Int] = {
    LazyList.from(2).filter(isPrime).take(n).toList
  }

  def main(args: Array[String]): Unit = {
    val N = readInt() // Change this to get more prime numbers
    println(s"First $N prime numbers: ${firstNPrimes(N).mkString(", ")}")
  }
}
