object exercise8 {

  val array: Array[Int] = Array(1,2,3,4,5)

  private def rotates(array: Array[Int], k: Int): Array[Int] =
    var newArray: Array[Int] = Array.empty
    for (i <- array.indices)
      newArray :+= array((i+k+1) % array.length)

    newArray




  def main(args: Array[String]): Unit =
    println(s"Array rotated by k: ${rotates(array, 2).mkString("(", ", ", ")")}")
}
