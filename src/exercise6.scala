

object exercise6 {

  var lazyList: LazyList[Int] = LazyList(2, 1, 4, 6, 3, 1, 5, 7, 8, 8, 2)

  private def secondGreatest(lazyList: LazyList[Int]) =
    //lazyList.sorted.takeRight(2).head
    lazyList.sorted(Ordering[Int].reverse)(1)


  def main(args: Array[String]): Unit = {
    println(s"Second greatest element: ${secondGreatest(lazyList)}")
  }
}
