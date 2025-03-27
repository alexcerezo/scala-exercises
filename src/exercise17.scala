class exercise17 (val name: String, val author: String, val yearPublished: Int ) {

  def isOlderThan(that: exercise17): Boolean =
    this.yearPublished < that.yearPublished
}

// Uso de la clase
object BookApp extends App {
  val book1 = new exercise17("Book1", "Author1", 2000)
  val book2 = new exercise17("Book2", "Author2", 2005)
  val book3 = new exercise17("Book3", "Author3", 2000)

  println(s"Book1 is older than Book2: ${book1.isOlderThan(book2)}")
  println(s"Book2 is not older than Book3: ${book2.isOlderThan(book3)}")
  println(s"Book1 is not older than Book3: ${book1.isOlderThan(book3)}")
}
