object exercise9 {

  private def sortArrays(array: Array[Int], array2: Array[Int]): Array[Int] =
    var newArray: Array[Int] = Array.empty
    // Sort two array without using sort method

    // Merge two arrays
    newArray = array ++ array2

    // Sort the merged array
    while (!isSorted(newArray))
      for (i <- 0 until newArray.length - 1)
        if (newArray(i) > newArray(i + 1))
          val temp = newArray(i)
          newArray(i) = newArray(i + 1)
          newArray(i + 1) = temp

    newArray

  private def isSorted(array: Array[Int]): Boolean =
    var boolean: Boolean = true
    for (i <- 0 until array.length - 1)
      if (array(i) > array(i + 1))
        boolean = false
    boolean

  private val array1: Array[Int] = Array(5, 3, 2, 5, 1)
  private val array2: Array[Int] = Array(6, 2, 9, 10, 23, 1)

  def main(args: Array[String]): Unit =
    println(s"Arrays sorted: ${sortArrays(array1, array2).mkString("(", ", ", ")")}")

}
