import scala.annotation.tailrec

object exercise28 extends App {
  @tailrec
  private def nth[A](list :List[A], n: Int): Option[A] =
    list match
      case Nil => None
      case x :: xs =>
        if (n == 0) Some(x)
        else nth(xs, n - 1)

  println(nth(List("a", "b", "c", "d", "e"), 2) == Some("c"))
  println(nth(List("a"), 2) == None)
  println(nth(Nil, 0) == None)

}