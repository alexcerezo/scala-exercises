
object exercise29 extends App{
  private def addSquares(list: List[Int]): Int =
    list.foldRight(0)((item: Int, acc: Int) => item * item + acc)

  println(addSquares(List(1, 2, 3, 4, 5)) == 55)
}
