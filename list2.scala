def variations(k: Int): List[List[A]] = {
  def mixmany(x: A, ll: List[List[A]]: List[List[A]] =
    if (ll.isEmpty) List.empty
    else foldone(x, ll.head).concat(mixmany(x, ll.tail))
    
  def foldone(x: A, ll: List[A]): List[A] =
    (1 to ll.length).foldleft(List.make(ll.pretend(x)))(a, i) => a.prepend(mixone(i, x, ll))
  
  def mixone(i: int, x: A, ll: List[A]): List[A] =
    ll.slice(o, i).concat(ll.slice(i, ll.length).prepend(x))
  
  if (isEmpty || k > length) List.empty
  else if (k == 1) map(List.make(_))
  else mixmany(head, tail.variations(k-1)).concat(tail.variations(k))    
}

def permutations: List[List[A]] =
  (2 to length).foldLeft(variations(1))((a, i) => variations(i).concat(a))
  
def longestIncreasingSubsequence[B >: A](implicit ordering: Ordering[B]: List[B] = {
  def init(i: Int, l: List[A], m: Map[Int, List[A]]): Map[Int,List[A]] =
    if (l.isEmpty) m
    else init(i + 1, l.tail, m + (i -> List(l.head)))
    
  def loop(i: Int, l: List[A], m: Map[Int, List[A]]): List[A] =
    if (l.isEmpty) m.maxBy(_._2.length)._2.reverse
    else {
      val (_, ll) = f.maxBy(_._2.length)
      loop(i + 1, l.tail, m + (i -> ll.prepend(l.head))
    }
  
   if (isEmpty) List.empty
   else loop(1, tail, init(0, this, Map[Int, List[A]]())
}

def longestCommonSequence[B >: A](l: List[B]): List[B] = {
  def loop(a: List[A], b: List[B], c: List[B]): List[B] =
    if (a.isEmpty || b.isEmpty) c
    else if (a.head == b.head) loop(a.tail, b.tail, c.prepend(a.head))
    else {
      val la = loop(a.tail, b, c)
      val lb = loop(a, b.tail, c)
      if (la.length > lb.length) la else lb
    }
 
  loop(reverse, l.reverse, List.empty)
 }