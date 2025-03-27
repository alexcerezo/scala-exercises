class exercise14 (point1: (Int, Int), point2: (Int, Int),point3: (Int, Int)) {

  private def distance(point1: (Int, Int), point2: (Int, Int)): Double =
    Math.sqrt(Math.pow(point2._1 - point1._1, 2) + Math.pow(point2._2 - point1._2, 2))

  def determineType(): String =
    val edge1 = distance(point1, point2)
    val edge2 = distance(point2, point3)
    val edge3 = distance(point3, point1)

    if (edge1 == edge2 && edge2 == edge3)
      "Equilateral"
    else if (edge1 == edge2 || edge2 == edge3 || edge1 == edge3)
      "Isosceles"
    else
      "Scalene"
}

// Uso de la clase
object TriangleApp extends App {
  val triangle1 = new exercise14((0, 0), (0, 0), (0, 0))
  val triangle2 = new exercise14((0, 0), (2, 2), (4, 0))
  val triangle3 = new exercise14((0, 0), (3, 0), (1, 2))

  println(s"Triangle 1 is: ${triangle1.determineType()}")
  println(s"Triangle 2 is: ${triangle2.determineType()}")
  println(s"Triangle 3 is: ${triangle3.determineType()}")
}
