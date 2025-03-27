import scala.annotation.tailrec

object exercise27 extends App{
  @tailrec
  private def last[A](list: List[A]): Option[A] =
    list match
      case Nil => None
      case x :: Nil => Some(x)
      case x :: xs => last(xs)

  println(last(List("a", "b", "c", "d")) == Some("d"))
  println(last(Nil) == None)
}
