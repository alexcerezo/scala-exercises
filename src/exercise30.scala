
object exercise30 extends App{
  private def reversed[A](list: List[A]): List[A] =
    list.foldLeft(List.empty[A])((acc, item) => item :: acc)


  println(reversed(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
}
