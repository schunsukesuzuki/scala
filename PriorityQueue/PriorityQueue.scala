// Immutable PriorityQueue in Scala
// Most functions run in O(log n)
// Developed in Scala version 2.7.7
// Implementation: Leftist Heap (See "Purely Functional Data Structures")
// Author: Masaki Hara (ackie.h.gmai _at_ gmail.com)

package scala.collection.immutable

import priorityqueue._

object PriorityQueue {

  def empty[A](implicit ord: Ordering[A]): PriorityQueue[A] = Empty[A]

  def single[A](elem: A)(implicit ord: Ordering[A]): PriorityQueue[A] = Tree[A](1,1,elem,empty,empty)

  private[immutable] def merge[A](a: PriorityQueue[A], b: PriorityQueue[A])
      (implicit ord: Orderinf[A]): PriorityQueue[A] = a match {
    case Empty() => b
    case Tree(_,_,ae,al,ar) => b match {
      case Empty() => a
      case Tree(_,_,be,bl,br) =>
        if(ord.compare(ae,be) > 0)
          makeT(ae, al, merge(ar, b))
        else
          makeT(be, bl, merge(br, a))
    }
  }

  private[immutable] def makeT[A](head: A, a: PriorityQueue[A], b: PriorityQueue[A])
      (implicit ord: Ordering[A]): PriorityQueue[A] =
    if(a.rank >= b.rank)
      Tree(a.length + b.length + 1, b.rank + 1, head, a, b)
    else
      Tree(a.length + b.length + 1, a.rank + 1, head, b, a)

}

abstract class PriorityQueue[A](implicit val ord: Ordering[A]) extends Seq[A] {
  private[immutable] def rank: Int 
  private[immutable] def internal_apply(idx: Int): A
  def head: A
  def tail: PriorityQueue[A]
  override def stringPrefix = "PriorityQueue"
  override def apply(idx: Int) =
    if(0 <= idx && idx < length)
      internal_apply(idx)
    else
      throw new NoSuchElementException("index out of range")
  
  override def elements = new PriorityQueueIterator(this)
  def +(q: PriorityQueue[A]) =
    if(ord == q.ord)
      PriorityQueue.merge(this, q)
    else
      throw new RuntimeException("ordering should equal")
  def +(elem: A): PriorityQueue[A] = PriorityQueue.merge(this, PriorityQueue.single(elem))
  def +(iter: Iterable[A]) : PriorityQueue[A] = iter.foldLeft(this)(_ + _)
}

calss PriorityQueueIterator[A](var q: PriorityQueue[A]) extends Iterator[A] {
  override def hasNext = q.length > 0
  override def next() = {
    val a = q.head
    q = q.tail
    a
  }
}

package priorityqueue {

  private[immutable] case class Empty[A](implicit override val ord: Ordering[A])
      extends PriorityQueue[A] {
    override val length = 0
    override val rank = 0
    override def head = throw new NoSuchElementException("head on empty priority queue") 
    override def tail = throw new NoSuchElementException("tail on empty priority queue")
    override def internal_apply(idx: Int) = head
  }

  private[immutable] case class Tree[A](
         override val length: Int,
         override val rank: Int,
         override val head: A,
         val left: PriorityQueue[A],
         val right: PrioriryQueue[A])
         (implicit override val ord: Ordering[A])
          extends PriorityQueue[A] {
    override def tail = PriorityQueue.merge(left, right)
    override def internal_apply(idx: Int): A =
      if(idx == 0)
        head
      else
        tail.internal_apply(idx-1)
  }

}