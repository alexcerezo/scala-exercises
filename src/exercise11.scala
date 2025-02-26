object exercise11 {

  private def runLenghtEncoding(string: String): String =
    var result = new StringBuilder
    var count = 1
    for (i <- 1 until string.length)
      if (string(i) == string(i-1))
        count += 1
      else
        result.append(string(i-1))
        result.append(count)
        count = 1

    result.append(string.last)
    result.append(count)

    result.toString

  private val string: String = "aaabbccc"


  def main(args: Array[String]): Unit =
    println(s"${runLenghtEncoding(string)}")

}
