object exercise35 extends App{

  private def flatten(list: List[Any]): List[Any] =
    list.foldRight(List.empty[Any])((item, acc) =>
      item match
        case sublist: List[_] => flatten(sublist) ::: acc
        case _ => item :: acc
    )
  println(flatten(List("a", List("b","c"), List("d", "e"))) == List("a", "b", "c", "d", "e"))
  println(flatten(List("a", List("b","c",List("d", "e")))) == List("a", "b", "c", "d", "e"))
}
