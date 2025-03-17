package lab2

trait ImmutableQueue[T] {
  def enqueue(elem: T): ImmutableQueue[T]
  def dequeue(): (Option[T], ImmutableQueue[T])
  def isEmpty: Boolean
}

class SimpleQueue[T] private (private val elems: List[T]) extends ImmutableQueue[T] {
  def this(elems: T*) =
    this(elems.toList)

  def enqueue(elem: T): ImmutableQueue[T] =
    new SimpleQueue(elems :+ elem) // Its equal to elems ::: List(elem) or elems ++ List(elem)


  def dequeue(): (Option[T], SimpleQueue[T]) =
    // Returns a tuple containing the first element and a new queue without that element
    elems match {
      case Nil => (None, this)
      case head :: tail => (Some(head), new SimpleQueue(tail))
    }

  def isEmpty: Boolean =
    elems.isEmpty

  override def toString: String =
    elems.toString()

  override def equals(obj: Any): Boolean =
    obj match {
      case that: SimpleQueue[T] => this.elems.equals(that.elems)
      case _ => false
    }

  override def hashCode(): Int =
    elems.hashCode()
}

@main def testSimpleQueue(): Unit = {
  val squeue = new SimpleQueue[Int]()
  val q = squeue.enqueue(1).enqueue(2).enqueue(3).enqueue(4)
  assert(q.dequeue() == (Some(1), SimpleQueue(2, 3, 4)), s"${q.dequeue()} should be equal to (1, SimpleQueue(List(2, 3, 4)))")
  assert(squeue.isEmpty, s"{q} should be empty")
  assert(!q.isEmpty, s"{q should not be empty")
  val q2 = SimpleQueue(1, 2, 3, 4)
  assert(q == q2, s"${q} and ${q2} should be equal")
  assert(q.hashCode() == q2.hashCode(), s"The hash codes of ${q} and ${q2} should be equal")
}
