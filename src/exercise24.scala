
object exercise24 extends App {
  
  private def eachWordCounter(text: String): Map[String, Int] = {
    var newText = text.replaceAll("\\s+", " ").split(" ").toList
    var wordCount = Map[String, Int]().withDefault(_ => 0)

    for (word <- newText)
      wordCount = wordCount.updated(word, wordCount(word) + 1)
    wordCount
  }
  
  println(eachWordCounter(readFile("C:/Users/alexc/Downloads/Basura/id.txt").toString))
}
