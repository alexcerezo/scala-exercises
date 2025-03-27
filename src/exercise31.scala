import scala.annotation.tailrec

object exercise31 extends App{

  @tailrec
  private def sum(x: Int, y: Int, function: (Int) => Int): Int =
    if (x > y)
      sum(y,x, function)
    else
      @tailrec
      def helper(x: Int, acc: Int): Int =
        if (x > y)
          acc
        else
          helper(x + 1, acc + function(x))

      helper(x, 0)

  println(sum(1, 5, x => x) == 15)

}
