package homework1

class Queue[A](/* ??? */):
  def push(a: A): Queue[A] = ???
  def pop: (A, Queue[A]) = ???

  def isEmpty: Boolean = ???
  def size: Int = ???

object Queue:
  def empty[A]: Queue[A] = ???

  def apply[A](xs: A*): Queue[A] = ???
