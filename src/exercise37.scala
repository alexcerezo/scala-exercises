object exercise37 extends App{

  private def pack[A](list: List[A]): List[List[A]] =
    list.foldRight(List.empty[List[A]]) { (item: A, acc: List[List[A]]) =>
      acc match {
        case (y :: ys) :: xs =>
          if (y == item)
            (item :: (y :: ys)) :: xs
          else
            List(item) :: acc
        case _ =>
          List(item) :: acc
      }
    }

  println(pack(List("a","a","a","b","c","c","d","e","e","e"))
    == List(List("a","a","a"), List("b"), List("c","c"),
    List("d"), List("e","e","e")))

}
