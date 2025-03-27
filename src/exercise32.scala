import scala.annotation.tailrec

object exercise32 extends App{

  private def listUntilN(n: Int): List[Int] =

        @tailrec
        def helper(n: Int, acc: List[Int]): List[Int] =
          if (n == 0)
            0 :: acc
          else
            helper(n-1, n :: acc)

        helper(n, List[Int]())

  println(listUntilN(25) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                  20, 21, 22, 23, 24, 25))


}
