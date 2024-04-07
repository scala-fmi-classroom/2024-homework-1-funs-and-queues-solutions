package homework1

class Queue[A] private (front: List[A], back: List[A]) extends Iterable[A]:
  def push(a: A): Queue[A] = new Queue(front, a :: back)
  def push(seq: Seq[A]): Queue[A] = seq.foldLeft(this)(_.push(_))

  def pop: (A, Queue[A]) =
    if this.isEmpty then throw new UnsupportedOperationException("Cannot pop from empty queue")
    else if front.isEmpty then new Queue(back.reverse, List.empty).pop
    else (front.head, new Queue(front.tail, back))

  override def isEmpty: Boolean = front.isEmpty && back.isEmpty
  override def size: Int = front.size + back.size

  def iterator: Iterator[A] = front.iterator ++ back.reverseIterator

object Queue:
  def empty[A]: Queue[A] = new Queue(List.empty, List.empty)

  def apply[A](xs: A*): Queue[A] = new Queue(xs.toList, List.empty)
