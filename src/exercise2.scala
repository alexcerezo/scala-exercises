import scala.io.StdIn.readInt

object exercise2 extends App {
  println("Welcome to your bank account")
  def ATM() =
    println("1. Check your balance")
    println("2. Make a deposit")
    println("3. Make a withdraw")
    println("4. Exit")
    print("Select an option: ")
    readInt()

  var money = 0
  var bool = true

  while (bool) {
    var option = ATM()
    if (option == 1) {
      println("Your balance is: " + money)
    } else if (option == 2) {
      println("How much do you want to deposit?")
      val deposit = readInt()
      money += deposit
      println("You deposited: " + deposit)
    } else if (option == 3) {
      println("How much do you want to withdraw?")
      val withdraw = readInt()
      if (withdraw > money) {
        println("You don't have enough money")
      } else{
        money -= withdraw
        println("You withdrew: " + withdraw)
      }
     } else if (option == 4) {
      println("Goodbye")
      bool = false
    }
    else {
      println("Invalid option")
    }
  }

}