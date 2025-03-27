import scala.annotation.tailrec

object exercise22 extends App {

  private def powerSet[A](s: Set[A]): Set[Set[A]] =
    @tailrec
    def powerSetTail(s: List[A], acc: Set[Set[A]]): Set[Set[A]] =
      s match
        case Nil => acc
        // Concatenate the accumulator with the accumulator plus the head of the list
        case x :: xs => powerSetTail(xs, acc ++ acc.map(z => z + x))
    powerSetTail(s.toList, Set(Set.empty))

  println(s"Power set of set with seven elements: ${powerSet(Set(1, 2, 3))}")
}

