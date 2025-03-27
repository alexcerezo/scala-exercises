

object exercise25 extends App {
  private def longestWord(path: String): String =
    val content = readFile(path).replaceAll("\\s+", " ")
    val words = content.split(" ")

    words.maxBy(_.length)

  println(longestWord("C:/Users/alexc/Downloads/Basura/id.txt"))
}