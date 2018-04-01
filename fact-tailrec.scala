//https://rn4ru.com/blog/posts/tail-recursion/
//http://bach.istc.kobe-u.ac.jp/lect/ProLang/org/scala-recursive.html

def fact(n: Int): BigInt = {
  def fact(n: Int, f: BigInt): BigInt =
    n match {
      case 0 => f
      case _ => fact(n - 1, n * f)
    }
  fact(n, 1)
}