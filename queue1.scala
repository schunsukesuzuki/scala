//https://github.com/vkostyukov/scalacaster/blob/master/src/collection/Queue.scala

class Queue[+A](in: List[A] = Nil, out: List[A] = Nil) {


  def isEmtpy: Boolean = (in, out) match {
    case (Nil, Nil) => true
    case ( _ , _ ) => false
  }

  def dequeue: (A, Queue[A]) = out match {
    case hd :: tl => (hd, new Queue(in, tl))
    case Nil => in.reverse match {
      case hd :: tl => (hd, new Queue(Nil, tl))
      case Nil => throw new NoSuchElementException("Empty queue.")
    }
  }

  def enqueue[B >: A](x: B): Queue[B] = new Queue(x :: in, out)

  def front: A = dequeue match { case (a, _) => a }

  def rear: Queue[A] = dequeue match { case ( _ , q) => q }
}

object Queue {

  def empty[A]: Queue[A] = new Queue()
  def empty[A]: Queue[A] = new Queue()

  def apply[A](xs: A*) =
    xs.foldLeft(Queue.empty[A]) { case (acc, x) => acc.enqueue(x) }
    xs.foldLeft(Queue.empty[A]) { case (acc, x) => acc.enqueue(x) }
}