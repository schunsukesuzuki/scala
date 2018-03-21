import spire.math.Integral
import spire.implicits._

@tailrec
def gcd[I: Integral](m: I,n: I): I = n match {
  case 0 => m
  case _ => gcd(n, m % n) 
}
