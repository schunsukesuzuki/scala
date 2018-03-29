//https://docs.scala-lang.org/tour/abstract-types.html

//Traits and abstract classes can have an abstract type member. This means that the concrete implementations define the actual type. Here’s an example:

trait Buffer {
  type T
  val element: T
}

abstract class SeqBuffer extends Buffer {
  type U
  type T <: Seq[U]
  def length = element.length
}

//Notice how we can use yet another abstract type U as an upper-type-bound. 
//This class SeqBuffer allows us to store only sequences in the buffer by stating that type T has to be a subtype of Seq[U] for a new abstract type U.

abstract class IntSeqBuffer extends SeqBuffer {
  type U = Int
}

// newIntSeqBuf is factory method
//Here the factory newIntSeqBuf uses an anonymous class implementation of IntSeqBuf (i.e. new IntSeqBuffer), setting type T to a List[Int].

def newIntSeqBuf(elem1: Int, elem2: Int): IntSeqBuffer =
  new IntSeqBuffer {
       type T = List[U]
       val element = List(elem1, elem2)
     }
val buf = newIntSeqBuf(7, 8)
println("length = " + buf.length)
println("content = " + buf.element)


abstract class Buffer[+T] {
  val element: T
}
abstract class SeqBuffer[U, +T <: Seq[U]] extends Buffer[T] {
  def length = element.length
}

////Note that we have to use variance annotations here (+T <: Seq[U]) in order to hide the concrete sequence implementation type of the object returned from method newIntSeqBuf

def newIntSeqBuf(e1: Int, e2: Int): SeqBuffer[Int, Seq[Int]] =
  new SeqBuffer[Int, List[Int]] {
    val element = List(e1, e2)
  }

val buf = newIntSeqBuf(7, 8)
println("length = " + buf.length)
println("content = " + buf.element)


