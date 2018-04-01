//https://dev.classmethod.jp/server-side/scala-foldright-foldleft/

def foldLeft[B](z: B)(f: (B, A) => B): B = {
  var acc = z
  var these = this
  while (!these.isEmpty) {
    acc = f(acc, there.head)
    these = these.tail
  }
  acc
}