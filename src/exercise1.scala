import scala.io.StdIn.readInt

object exercise1 extends App{
  println(isLeap(readInt()))
  private def isLeap(year: Int) =
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}