object exercise12 {

  private def mostCommonChar(string: String): Char =
    var maxChar: Char = ' '
    var maxCount = 0

    for (i <- 0 until string.length)
      var count = 0
      var char: Char = string(i)
      for (j <- 0 until string.length)
        if (string(i) == string(j))
          count += 1

      if (count > maxCount)
        maxCount = count
        maxChar = char

    maxChar


  private val string: String = "aaabbcccdddddc"


  def main(args: Array[String]): Unit =
    println(s"${mostCommonChar(string)}")

}
