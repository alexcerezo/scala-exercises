class exercise13 (var balance: Double) {

  def deposit(amount: Double): Unit = {
    if (amount > 0) {
      balance += amount
    }
  }

  def withdraw(amount: Double): Unit = {
    if (amount > 0 && amount <= balance) {
      balance -= amount
    }
  }

  def getBalance: Double = balance
}

// Uso de la clase
object BankApp extends App {
  val account = new exercise13(.0)
  account.deposit(500.0)
  println(s"Saldo actual: ${account.getBalance}")
  account.withdraw(200.0)
  println(s"Saldo actual: ${account.getBalance}")
  account.withdraw(1500.0) // Intento de retiro mayor al saldo
  println(s"Saldo final: ${account.getBalance}")
}
