import scala.annotation.tailrec

object exercise20 extends App {

    private def fibonacci(n: Int): Int =
        @tailrec
        def fibonacciTail(n: Int, a: Int, b: Int): Int =
            n match
                case 0 => a
                case 1 => b
                case _ => fibonacciTail(n - 1, b, a + b)

        fibonacciTail(n, 0, 1)

    println(s"Fibonacci of 0: ${fibonacci(0)}")
    println(s"Fibonacci of 1: ${fibonacci(1)}")
    println(s"Fibonacci of 2: ${fibonacci(2)}")
    println(s"Fibonacci of 3: ${fibonacci(3)}")
    println(s"Fibonacci of 4: ${fibonacci(4)}")
    println(s"Fibonacci of 5: ${fibonacci(5)}")
    println(s"Fibonacci of 6: ${fibonacci(6)}")
    println(s"Fibonacci of 7: ${fibonacci(7)}")
    println(s"Fibonacci of 8: ${fibonacci(8)}")
    println(s"Fibonacci of 9: ${fibonacci(9)}")
    println(s"Fibonacci of 10: ${fibonacci(10)}")
    
}

