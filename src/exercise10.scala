object exercise10 {

  private def checkPermutation(array1: Array[Int], array2: Array[Int]): Boolean = {
    if (array1.length != array2.length) false
    else array1.sorted.sameElements(array2.sorted)
  }

  private val array1: Array[Int] = Array(1,2,3,4,5)
  private val array2: Array[Int] = Array(5,4,3,2,1)
  private val array3: Array[Int] = Array(5,4,3,2,2)

  def main(args: Array[String]): Unit = {
    println(s"Arrays are permutations: ${checkPermutation(array1, array2)}")
    println(s"Arrays are permutations: ${checkPermutation(array1, array3)}")
  }
}
