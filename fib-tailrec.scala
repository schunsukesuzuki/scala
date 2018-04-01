//http://bach.istc.kobe-u.ac.jp/lect/ProLang/org/scala-recursive.html

  def fib(n: Int): BigInt = {
  def fib(n: Int, f0: BigInt, f1: BigInt): BigInt =
    n match {
      case 0 => f0
      case _ => fib(n - 1, f1, f0 + f1)
    }
  fib(n, 0, 1)
}