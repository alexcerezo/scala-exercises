import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object exercise39 extends App{

  private def easyRange(n: Int, m: Int): List[Int] =
    if n > m then
      (m to n).toList.reverse
    else
      (n to m).toList

  private def recursiveRange(n: Int, m: Int): List[Int] =

    @tailrec
    def helper(n: Int, m: Int, acc: List[Int]): List[Int] =
      if !(n == m) then helper(n-1, m, acc :+ n)
      else acc

    if n > m then
      helper(n, m, List.empty[Int]) :+ m
    else if m > n then
      n :: helper(m, n, List.empty[Int]).reverse
    else
      List.empty[Int]



  private def bufferRange(n: Int, m: Int): List[Int] = {
    val listBuffer = ListBuffer.empty[Int]

    if n > m then
      for i <- m to n do
        listBuffer.prepend(i)
    else if m > n then
      for i <- n to m do
        listBuffer.append(i)

    listBuffer.toList
  }

  private def lazyListRange(n: Int, m: Int): List[Int] =
    if n > m then
      LazyList.range(m, n + 1).reverse.toList
    else if m > n then
      LazyList.range(n, m + 1).toList
    else List.empty[Int]

  private def range(n: Int, m: Int): List[Int] =
    if n > m then
      List.range(m, n + 1).reverse
    else if m > n then
      List.range(n, m + 1)
    else List.empty[Int]



  println(range(4, 9) == List(4, 5, 6, 7, 8, 9))
  println(range(9, 4) == List(9, 8, 7, 6, 5, 4))
  println(range(5, 5) == List())

}
