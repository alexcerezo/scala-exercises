object exercise7 {
  val array: Array[Int] = Array(2, 1, 4, 6, 3, 1, 5, 7, 8, 8, 2)

  private def removeDuplicates(array: Array[Int]): Array[Int] =
    var newArray: Array[Int] = Array.empty
    for (i <- array)
      if (!newArray.contains(i))
        newArray :+= i
    newArray

  def main(args: Array[String]): Unit =
    println(s"Array without duplicates: ${removeDuplicates(array).mkString("(", ", ", ")")}")
}