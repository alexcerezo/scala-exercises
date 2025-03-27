def sum(l:List[Int]):Int =
  l.foldRight(0)(_+_)

def product(l:List[Int]):Int =
  l.foldRight(1)(_*_)

def length[A](l:List[A]):Int =
  l.foldRight(0)((item, acc) => acc + 1)

def reverse[A](l:List[A]):List[A] =
  l.foldLeft(List[A]())((acc, item) => item :: acc)

def append[A](l1:List[A],l2:List[A]):List[A] =
  if  (length(l2) > length(l1))
    l1.foldRight(l2)((item, acc) => item :: acc)
  else
    l2.foldRight(l1)((item, acc) => item :: acc)

def exists[A](l:List[A],f:A=>Boolean):Boolean =
  l.foldLeft(false)((acc, item) => acc || f(item))

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


sum(List(1,2,3)) == 6
product(List(1,3,5)) == 15
length(List("Hola", " ", "Mundo")) == 3
reverse(List(1,2,3)) == List(3,2,1)
append(List(1,2,3),List(1,2)) == List(1,2,1,2,3)
append(List(1,2), List(1,2,3)) == List(1,2,1,2,3)
exists(List(1,2,3),_>2) == true
exists(List("Hola", "Mundo"),_.length>=5) == true
exists(List("Hola", "Mundo"),_.length<3) == false
remdups(List(1,1,3,3,3,2,1,2,2,1,2)) == List(1, 3, 2, 1, 2, 1, 2)
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



