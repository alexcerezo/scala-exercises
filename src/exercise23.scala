import scala.annotation.tailrec

object exercise23 extends App {
  private def subsequence(list: List[Int]): List[Int] =
    @tailrec
    def subsequenceHelper(list: List[Int], acc: List[Int], max: List[Int]): List[Int] =
      list match
        case Nil => max
        case x :: xs =>
          if (x > acc.last)
            subsequenceHelper(xs, acc :+ x, if (acc.length + 1 > max.length) acc else max)
          else
            subsequenceHelper(xs, List(x), if (acc.length > max.length) acc else max)

    subsequenceHelper(list.tail, List[Int](list.head), List[Int](list.head))

  println(s"Subsequence of list with seven elements: ${subsequence(List(1, 2, 3, 4, 5, 6, 7, 3, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 5, 6, 7, 8))}")
}