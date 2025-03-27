import scala.annotation.tailrec

// Exercise 1
def primeFactors(n: Int): List[Int] =
  @tailrec
  def helper(n: Int, m: Int, acc: List[Int]): List[Int] =
    if n % m != 0 then
      helper(n, m + 1, acc)
    else if n == m then
      acc :+ m
    else
      helper(n / m, m, acc :+ m)

  helper(n, 2, List.empty[Int])

println(primeFactors(60) == List(2, 2, 3, 5))
println(primeFactors(97) == List(97))
println(primeFactors(84) == List(2, 2, 3, 7))
println(primeFactors(315) == List(3, 3, 5, 7))

// Exercise 2
def binarySearch(arr: Array[Int], elt: Int): Option[Int] =

  @tailrec
  def helper(n: Int, m: Int, arr: Array[Int], elt: Int): Option[Int] =
    if n > m then
      None
    else
      val mid = n + (m - n) / 2
      if arr(mid) == elt then
        Some(mid)
      else if arr(mid) > elt then
        helper(n, mid - 1, arr, elt)
      else helper(mid + 1, m, arr, elt)


  helper(0, arr.length - 1, arr, elt)

val arr = Array(1, 3, 5, 7, 9, 11)
println(binarySearch(arr, 5)) // Output: Some(2)
println(binarySearch(arr, 10)) // Output: None

// Exercise 3 or exercise 33 in the exercises file or exercise 5 in Lab3
def unzip[A](l:List[(A,A)]):(List[A],List[A]) =
  l.foldRight((List.empty[A], List.empty[A]))((item, acc) => (item._1 :: acc._1, item._2 :: acc._2))

def unzipTailRecursive[A](list: List[(A,A)]): (List[A], List[A]) =
  @tailrec
  def helper(list: List[(A,A)], acc: (List[A], List[A])): (List[A], List[A]) =
    list match
      case Nil => acc
      case x :: xs => helper(xs, (acc._1 :+ x._1, acc._2 :+ x._2))

  helper(list, (List.empty[A], List.empty[A]))


unzip(List((1, 2), (3, 4), (5, 6))) == (List(1, 3, 5), List(2, 4, 6))
unzipTailRecursive(List((1, 2), (3, 4), (5, 6))) == (List(1, 3, 5), List(2, 4, 6))
unzipTailRecursive(List((1, 2), (3, 4), (5, 6))) == unzip(List((1, 2), (3, 4), (5, 6)))
unzip(List((1,'a'),(2,'b'),(3,'c'))) == (List(1,2,3), List('a','b','c'))

// Exercise 4 or exercise 34 in the exercises file
def zip[A](tuple: (List[A], List[A])): List[(A, A)] =
  @tailrec
  def helper(tuple: (List[A], List[A]), acc: List[(A, A)]): List[(A, A)] =
    tuple match
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (x :: xs, y :: ys) => helper((xs, ys), acc :+ (x, y))
  helper(tuple, List.empty[(A, A)])

zip(List(10, 20, 30), List('a', 'b', 'c')) == List((10, 'a'), (20, 'b'), (30, 'c'))
zip(List(10, 20, 30), List('a', 'b')) == List((10,'a'), (20,'b'))

// Exercise 5
def filter(l: List[Int], f: Int => Boolean): List[Int] =
  l.foldRight(List.empty[Int])((item, acc) => if f(item) then item :: acc else acc)

filter(List(1,2,3,4,5), _ % 2 == 0 ) == List(2,4)

// Exercise 6
def map(l:List[Int], f: Int => Int): List[Int] =
  l.foldRight(List.empty[Int])((item, acc) => f(item) :: acc)

map(List(1,2,3,4,5), _ * 2 ) == List(2,4,6,8,10)

// Exercise 7
def groupBy[A,B](l: List[A], f: A => B): Map[B, List[A]] =
  l.foldRight(Map.empty[B, List[A]])((item, acc) =>
    val key = f(item)
    acc.get(key) match
      case None => acc + (key -> List(item))
      case _ => acc.updated(key, item :: acc(key))
  )

groupBy(List(1,2,3,4,5), _ % 2 == 0) == Map(false -> List(1,3,5), true -> List(2,4))

// Exercise 8
def reduce[A](list: List[A], f: (A, A) => A): A =
  list.tail.foldRight(list.head)((item, acc) => f(item, acc))

reduce(List(1,2,3,4,5), _ * _) == 120

// Exercise 9 or exercise 22 in the exercises file
def subsets[A](s: Set[A]): Set[Set[A]] =
  @tailrec
  def helper(s: List[A], acc: Set[Set[A]]): Set[Set[A]] =
    s match
      case Nil => acc
      case x :: xs => helper(xs, acc ++ acc.map(z => z + x))
  helper(s.toList, Set(Set.empty))

subsets(Set(1, 2, 3)) == Set(Set(),Set(1),Set(2),Set(1,2),Set(3),Set(1,3),Set(2,3),Set(1,2,3))

// Exercise 10
def generateParentheses(n: Int, opened: Int = 1, closed: Int = 0, current: String = "("): List[String] = {
  if (current.length == 2 * n - 1) {
    return List(current + ")") // Asegurar que termine en 0
  }

  var result = List[String]()

  // Agregar un 1 si no hemos agotado los n unos
  if (opened < n) {
    result = result ++ generateParentheses(n, opened + 1, closed, current + "(")
  }

  // Agregar un 0 si no hemos agotado los n ceros y no hay más ceros que unos
  if (closed < n && closed < opened) {
    result = result ++ generateParentheses(n, opened, closed + 1, current + ")")
  }

  result
}

generateParentheses(5)






