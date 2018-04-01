//https://dev.classmethod.jp/server-side/scala-foldright-foldleft/

def foldRight[B](zero: B)(f: (A, B) => B): B =
  if (this.isEmpty) zero
  else f(head, tail.foldRight(zero)(f))