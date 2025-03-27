import scala.annotation.tailrec

object exercise21 extends App {
  private def sum(n: Int): Int =
    @tailrec
    def sumHelper(n: Int, acc: Int): Int =
      n match
        case 0 => acc
        case _ => sumHelper(n / 10, acc + n % 10)

    sumHelper(n, 0)

  val number = 1234
  println(s"${sum(number)}")
}
