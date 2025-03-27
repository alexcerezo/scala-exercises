import scala.io.Source.fromFile;

def readFile(path: String): String = {
  val source = fromFile(path)
  var x = ""
  try {
    x = source.mkString("")
  } finally {
    source.close()
  }
  x
}

object reader extends App {
  println(readFile("C:/Users/alexc/Downloads/Basura/id.txt"))
}