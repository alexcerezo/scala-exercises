import scala.annotation.tailrec
import scala.io.StdIn.readInt
object exercise5 extends App {
  private val N = readInt()
  private val M = readInt()
  lcmandgcd(N, M)

  @tailrec
  private def lcm(n: Int, m: Int): Int = {
    if (m == 0) n
    else lcm(m, n % m)
  }

  private def gcd(n: Int, m: Int): Int =
    n * m / lcm(n, m)

  private def lcmandgcd(n: Int, m: Int): Unit =
    println((lcm(n, m), gcd(n, m)))
}