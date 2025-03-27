object exercise38 extends App{

  private def replicate[A](list: List[A], n: Int): List[A] =
    list.foldRight(List.empty[A])((item, acc) => List.fill(n)(item) ::: acc)

  println(replicate(List("a", "b", "c", "d"), 3) == List("a","a","a","b","b","b","c","c","c","d","d","d"))
}
