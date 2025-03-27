import scala.io.StdIn.readLine

object exercise18 extends App {
  private def userInput(): Int =
    var validInput = false
    var number = 0
    var input: String = ""

    while (!validInput)
      input = readLine("Enter a number: ")
      try
        number = input.toInt
        validInput = true
      catch
        case e: NumberFormatException =>
          println("Invalid input.")
    number

  val number = userInput()
  println(s"Number: $number")
}
