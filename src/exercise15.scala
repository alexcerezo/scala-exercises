class exercise15 (val name: String, val age: Int, val grades: List[Int]){

  def average(): Double = grades.sum / grades.length

}

// Uso de la clase
object StudentApp extends App {
  val student1 = new exercise15("John", 20, List(90, 80, 70, 60, 50))
  val student2 = new exercise15("Jane", 22, List(100, 90, 80, 70, 60))
  val student3 = new exercise15("Jim", 21, List(80, 70, 60, 50, 40))

  println(s"${student1.average()}")
  println(s"${student2.average()}")
  println(s"${student3.average()}")
}
