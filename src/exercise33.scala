import scala.annotation.tailrec

object exercise33 extends App{

  private def unzip[A](list: List[(A,A)]): (List[A], List[A]) =
    list.foldRight((List.empty[A], List.empty[A]))((item, acc) => (item._1 :: acc._1, item._2 :: acc._2))


  private def unzipTailRecursive[A](list: List[(A,A)]): (List[A], List[A]) =
    @tailrec
    def helper(list: List[(A,A)], acc: (List[A], List[A])): (List[A], List[A]) =
      list match
        case Nil => acc
        case x :: xs => helper(xs, (acc._1 :+ x._1, acc._2 :+ x._2))

    helper(list, (List.empty[A], List.empty[A]))


  println(unzip(List((1, 2), (3, 4), (5, 6))) == (List(1, 3, 5), List(2, 4, 6)))
  println(unzipTailRecursive(List((1, 2), (3, 4), (5, 6))) == (List(1, 3, 5), List(2, 4, 6)))
  println(unzipTailRecursive(List((1, 2), (3, 4), (5, 6))) == unzip(List((1, 2), (3, 4), (5, 6))))
}
