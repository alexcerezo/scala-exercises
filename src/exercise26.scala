import java.io._

object exercise26 extends App {

  private def writeFile(name: String, source: String): Unit =
    val file = new File(name)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(readFile(source))
    bw.close()
    println(s"Content written to $name.")

  writeFile("C:/Users/alexc/Downloads/Basura/a.txt", "C:/Users/alexc/Downloads/Basura/id.txt")
}