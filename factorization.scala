import scala.collection.mutable.ArrayBuffer

object Factorization {
  def main (args: Array[String)]: Unit = {
    val random = scala.util.Random
    val numbers = 
      if (args.isEmpty) {
        (1 to 5).map(i => randam.nextInt(Int.MaxValue))
      } else {
        // Array to Vector
        args.map(i => i.toInt).to[collection.immutable.seq]
      }
  }
  
  private def factorize(number: Int) : List[Int] = {
    val list = new Arraybuffer[ Int ]
    var n = number
    var f = 2
    
    while (n != 1) {
      if (n % f == 0) {
        list += f
        n /= f
      } else {
        f += 1
      }
    }
    
    println("Thread:%3d factorize %10d to %s.format(Thread.currentThread.getId, number, list.mkString(", ")))
    list.toList
  }
  
}