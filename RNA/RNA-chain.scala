final class RNA private (val groups: Array[Int], val length: Int)
  extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA] {
  
  import RNA._
  
  override protected[this] def newbuilder: Builder[base, RNA] = 
    RNA.Newbuilder
  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundException
    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }
  
  override def foreach[U](f:Base => U): Unit = {
    var i = 0
    var b = 0
    while (i < length) {
      b = if (i % N == 0) groups(i / N) else b >>> S
      f(Base.fromInt(B & M))
      i += 1
    }
  }
  
}