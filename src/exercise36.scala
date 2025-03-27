object exercise36 extends App {

  private def compress[A](list: List[A]): List[A] =
    list.foldRight(List.empty[A])((item, acc) =>
      acc match
        case x :: xs => if x.equals(item) then acc else item :: acc
        case _ => item :: acc
    )

  println(compress(List("a", "a", "a", "b", "c", "c", "d", "a", "e", "e", "e")) == List("a", "b", "c", "d", "a", "e"))

}
