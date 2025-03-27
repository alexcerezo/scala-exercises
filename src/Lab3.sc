import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.HashMap

// Exercise 1
def sum(l:List[Int]):Int =
  l.foldRight(0)(_+_)

def product(l:List[Int]):Int =
  l.foldRight(1)(_*_)

def length[A](l:List[A]):Int =
  l.foldRight(0)((item, acc) => acc + 1)

sum(List(1,2,3)) == 6
product(List(1,3,5)) == 15
length(List("Hola", " ", "Mundo")) == 3

// Exercise 2
def reverse[A](l:List[A]):List[A] =
  l.foldLeft(List[A]())((acc, item) => item :: acc)

def append[A](l1:List[A],l2:List[A]):List[A] =
  if  (length(l2) > length(l1))
    l1.foldRight(l2)((item, acc) => item :: acc)
  else
    l2.foldRight(l1)((item, acc) => item :: acc)

reverse(List(1,2,3)) == List(3,2,1)
append(List(1,2,3),List(1,2)) == List(1,2,1,2,3)
append(List(1,2), List(1,2,3)) == List(1,2,1,2,3)

// Exercise 3
def exists[A](l:List[A],f:A=>Boolean):Boolean =
  l.foldLeft(false)((acc, item) => acc || f(item))

exists(List(1,2,3),_>2) == true
exists(List("Hola", "Mundo"),_.length>=5) == true
exists(List("Hola", "Mundo"),_.length<3) == false

// Exercise 4
def f1(l: List[Int]): List[Int] =
  l.foldRight(List.empty[Int])((item, acc) => if item < 0 then -item :: acc else acc)

def f2(l: List[Int]): List[Int] =
  @tailrec
  def helper(l: List[Int], acc: List[Int]): List[Int] =
    l match
      case Nil => acc
      case x :: xs =>
        if x < 0 then
          helper(xs, acc :+ -x)
        else
          helper(xs, acc)

  helper(l, List.empty[Int])

def f3(l: List[Int]): List[Int] =
  l.filter(_ < 0).map(-_)


f1(List(1,-2,3,-4,-5,6)) == List(2,4,5)
f2(List(1,-2,3,-4,-5,6)) == List(2,4,5)
f3(List(1,-2,3,-4,-5,6)) == List(2,4,5)

// Exercise 5 or exercise 33 in the exercises file
def unzip[A](l:List[(A,A)]):(List[A],List[A]) =
  l.foldRight((List.empty[A], List.empty[A]))((item, acc) => (item._1 :: acc._1, item._2 :: acc._2))

unzip(List((1,'a'),(2,'b'),(3,'c'))) == (List(1,2,3), List('a','b','c'))

// Exercise 6

def compose[A](fs:List[A=>A], x:A):A =
  fs.foldRight(x)((f, acc) => f(acc))

compose(List[Int => Int](Math.pow(_,2).toInt, _+2), 5) == 49

// Exercise 7
def remdups[A](l:List[A]):List[A] =
  l.foldRight(List[A]())( (item, acc) =>
    acc match
      case Nil => List[A](item)
      case x :: xs =>
        if x == item then
          acc
        else
          item :: acc
  )

remdups(List(1,1,3,3,3,2,1,2,2,1,2)) == List(1, 3, 2, 1, 2, 1, 2)

// Exercise 8
def fibonacci(n:Int):Int =
  (1 to n).foldRight((0,1))((_, acc) => (acc._2, acc._1 + acc._2))._1

fibonacci(5) == 5
fibonacci(10) == 55

// Exercise 9. Similar to exercise 22 on the exercises file.
def inits[A](l: List[A]): List[List[A]] =
  l.foldRight(List(List.empty[A]))(
    (item, acc) =>
      Nil :: acc.map(item :: _)
  ) // Simplest way to do it

def inits1[A](l:List[A]): List[List[A]] =
l.foldRight(List(List.empty[A]))(
  (item, finalList) =>
    finalList.foldRight(List(List.empty[A]))(
      (subsquence, futurePreffixList) =>
        (item :: subsquence) :: futurePreffixList
    )
).reverse

def inits2[A](l:List[A]): List[List[A]] =
  l.foldRight(List(List.empty[A]))(
    (item, acc) => acc.foldLeft(List(List.empty[A]))((acc2, item2) => (item :: item2) :: acc2)
  ).sortBy(_.length)

inits(List(1,2,3)) == List(List(),List(1),List(1,2),List(1,2,3))
inits(List(3)) == List(List(),List(3))
inits(List()) == List(List())
inits1(List(1,2,3))== List(List(),List(1),List(1,2),List(1,2,3))
inits1(List(3)) == List(List(),List(3))
inits1(List()) == List(List())
inits2(List(1,2,3)) == List(List(),List(1),List(1,2),List(1,2,3))
inits2(List(3)) == List(List(),List(3))
inits2(List()) == List(List())

// Exercise 10
def halfEven(l1:List[Int],l2:List[Int]):List[Int] =

  @tailrec
  def helper(l1:List[Int],l2:List[Int], acc: List[Int]): List[Int] =
    (l1,l2) match
      case (Nil, Nil) => acc
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (x :: xs, y :: ys) =>
        if (x+y) % 2 == 0 then
          helper(xs, ys, (x+y)/2 :: acc)
        else
          helper(xs, ys, acc)

  helper(l1,l2,List.empty[Int]).reverse

def halfEven1(l1:List[Int],l2:List[Int]):List[Int] =
  l1.zip(l2).map{case (x,y) => (x+y)/2}.filter(_ % 2 == 0)

halfEven(List(1,2,3,4),List(3,6,4)) == List(2, 4)
halfEven1(List(1,2,3,4),List(3,6,4)) == List(2, 4)

// Exercise 11
val logs = List(
  "ERROR: Null pointer exception",
  "INFO: User logged in",
  "ERROR: Out of memory",
  "WARNING: Disk space low",
  "INFO: File uploaded",
  "ERROR: Database connection failed"
)

logs.foldRight(mutable.HashMap.empty[String, Int])((item, acc) => {
  val key = item.split(":")(0)
  acc += (key -> (acc.getOrElse(key, 0) + 1))
})

logs.foldRight(List.empty[String])((item, acc) => if item.contains("ERROR") then item :: acc else acc)

// Exercise 12
val sales = List(
  ("Laptop", 2, 1000.0),
  ("Mouse", 10, 15.0),
  ("Keyboard", 5, 50.0),
  ("Monitor", 3, 200.0),
  ("USB Drive", 20, 4.0)
)

sales.foldLeft(0.0)((acc, item) => acc + item._2 * item._3)

sales.foldRight(List.empty)((item, acc) =>
  if item._2 * item._3 > 100 then
    (item._1, item._2 * item._3) :: acc
  else
    acc
)

// Exercise 13
val sentences = Set(
  "Scala is a functional language",
  "The power of functional programming is great",
  "Functional programming is elegant"
)
val stopWords = Set("a", "the", "is", "of")


sentences.map(_.toLowerCase)
  .flatMap(_.split(" "))
  .filter(! stopWords.contains(_))
  .toSet

// Exercise 14
val words = List("scala", "is", "awesome", "scala", "functional", "scala",
  "is", "great")

words.foldRight(HashMap.empty[String, Int])((item, acc) =>
  acc += (item -> (acc.getOrElse(item, 0) + 1))
)

// Exercise 15
val warehouse1 = Map("laptop" -> 5, "mouse" -> 20, "keyboard" -> 10)
val warehouse2 = Map("laptop" -> 3, "mouse" -> 15, "monitor" -> 8)

(warehouse1.keySet ++ warehouse2.keySet).map { key =>
  val v1 = warehouse1.getOrElse(key, 0)
  val v2 = warehouse2.getOrElse(key, 0)
  key -> (v1 + v2)
}.toMap


