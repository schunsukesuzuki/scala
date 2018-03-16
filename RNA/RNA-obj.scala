object RNA {
  private val S = 2
  private val M = (1 << S) - 1
  private val N = 32 / S
  def fromseq(buf: Seq[Base]): RNA = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i < 0 until buf.length)
      groups(1 / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA(groups, buf.length)
  }
  def apply(bases: Base*) = fromSeq(bases)
  
  def newBuilder: Builder[Base, RNA] =
    new ArrayBuffer mapResult fromseq
    
  implicit def canBuildFrom: CanBuildFrom[RNA, Base, RNA] =
    new CanBuildFrom[RNA, Base, RNA] {
      def apply(): Builder[Base,RNA] = newBuilder
      def apply(from: RNA): Builder[Base, RNA] = newBuilder
  }
  
  
}