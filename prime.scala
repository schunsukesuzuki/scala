import spire.math.Integral
import spire.implicits._

object PrimeNumber{

  /** n番目の素数 */
  def apply[I: Integral](n: I): I = streamOf.apply(n.toInt)

  /**　素数列 */
  def stream: Stream[Int] = streamOf[Int]

  /** 素数か判定 */
  def isPrime[I: Integral](n: I): Boolean =
    n > 1 && streamOf.takeWhile(p => p*p <= n).forall(n % _ != 0)
}