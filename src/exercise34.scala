import scala.annotation.tailrec

object exercise34 extends App {
  private def zip[A](tuple: (List[A], List[A])): List[(A, A)] =
    @tailrec
    def helper(tuple: (List[A], List[A]), acc: List[(A, A)]): List[(A, A)] =
      tuple match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (x :: xs, y :: ys) => helper((xs, ys), acc :+ (x, y))
    helper(tuple, List.empty[(A, A)])

  println(zip(List(10,20,30), List('a','b','c')) == List((10,'a'),(20,'b'),(30,'c')))
  println(zip(List(10,20,30), List('a','b')) == List((10,'a'),(20,'b')))
}
