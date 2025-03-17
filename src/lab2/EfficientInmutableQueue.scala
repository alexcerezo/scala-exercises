package lab2

class EfficientQueue[T] private (private val front: List[T], private val rearReverse: List[T]) extends ImmutableQueue[T] {
  def this(p: T*) = this(p.toList, Nil)

  def enqueue(elem: T): ImmutableQueue[T] =
    new EfficientQueue(front, elem :: rearReverse)

  def dequeue(): (Option[T], EfficientQueue[T]) =
    front match
    case Nil => rearReverse.match
      case Nil => (None, this) // Return None if both front and rear are empty
      case _ => new EfficientQueue[T](rearReverse.reverse, Nil).dequeue() // If front is empty, reverse rear and dequeue
    case head :: tail => (Some(head), new EfficientQueue(tail, rearReverse)) // Return the head of the front and the rest as the new queue


  // Method to check if the queue is empty
  def isEmpty: Boolean =
    front.isEmpty && rearReverse.isEmpty

  // Override toString method to provide a string representation of the queue
  override def toString: String = (front ++ rearReverse.reverse).toString().replace("List", "EfficientQueue")

  // Override equals method to compare two queues
  override def equals(obj: Any): Boolean = obj match {
    case that: EfficientQueue[T] =>
      (this.front ++ this.rearReverse.reverse).equals(that.front ++ that.rearReverse.reverse)
    case _ => false
  }

  // Override hashCode method to provide a hash code for the queue
  override def hashCode(): Int = (front ++ rearReverse.reverse).hashCode()
}

@main def testImmutableQueue(): Unit = {
  val squeue = new EfficientQueue[Int]()
  val q = squeue.enqueue(1).enqueue(2).enqueue(3).enqueue(4)
  assert(q.dequeue() == (Some(1), EfficientQueue(2, 3, 4)), s"${q.dequeue()} should be equal to (1, SimpleQueue(List(2, 3, 4)))")
  assert(squeue.isEmpty, s"{q} should be empty")
  assert(!q.isEmpty, s"{q should not be empty")
  val q2 = EfficientQueue(1, 2, 3, 4)
  assert(q == q2, s"${q} and ${q2} should be equal")
  assert(q.hashCode() == q2.hashCode(), s"The hash codes of ${q} and ${q2} should be equal")
}
