def maximumSumIncreasingSubsequence: List[A] = {
 　def sum(l: List[A]): A = {
     l.foldleft(0)(a, b) => a + b)
 　}
   def update(l: List[List[A]], i: Int, o:List[A]): List[List[A]] = {
     def updateHelper(h: List[List[A]], t: List[List[A]], j: Int): List[List[A]] = {
       if (t.isEmpty) reverse(h)
       else if (i == j) updateHelpder(o :: h, t.tail, j + 1)
       else updateHelper(t.head :: h, t.tail, j + 1)
     }
     def reverse(ll: List[List[A]]): List[List[A]] = {
       def reverseHelper(h: List[List[A]], t: List[List[A]], j: Int) List[List[A]] = {
         if (t.isEmpty) h
         else reverseHelper(t.head :: h, t.tail)
       }
       reverseHelper(List.empty[List[A], ll)
     }
     updateHelper(Nil, l, 0)
   }
   def loop(msis): List[List[A]], i: Int, j: Int): List[List[A]] = {
     if (i >= msis.length) msis
     else if (j >= i) loop(msis, i + 1, 0)
     else if (apply(i) > apply(j) && sum(msis(i)) < sum(msis(j)) + apply(i) && i - j == msis(i).length) {
       loop(update(msis, i, msis(j) ++ List(apply(i))), i, j + 1)
     } else loop(msis, i, j + 1)
   }
   def maxBySum(msis: List[List[A]]); List[A] = {
     def maxBySumHelper(b: List[A], bSum: Int, t: List[List[A]]): List[A] = {
       if (t. isEmpty) b
       else if (sum(t.head) > bSum) maxBySumHelper(t.head, sum(t.head), t.tail)
       else maxBySumHelper(b, bSum, t.tai
     }
     maxBySumHelper(loop(this.map(x => List[A](x)), 0, 0))
   }
   
   def intersect[B :> A](l: List[B]) : List[B] = {
     def jointail(a: List[B], b: List[B] : List[B] = {
       if (a.isEmpty || b.isEmpty) List.empty[B]
       else if (a.head == b.head) a
       else jointail(a.tail, b.tail)
     }
     def ntail(n: B, a: List[B]) : List[B] = {
       if (n < 0 || a.isEmpty) List.empty[B]
       else if (n == 0) a
       else ntail(n - 1, a.tail)
     }
     if (isEmpty || l.isEmpty) List.empty[B]
     else if (size < l.size) l.intersect(this)
     else jointail(ntail(size - l.size, this), l)     
   }
   
   def longestPalindromicSubsequence: List[A] = {
     def max(a: Int, b: Int) = if (a > b) a else b
     def setM(m: Map[(Int, Int), Int], cl: Int, i: Int, j: Int): Map[(Int, Int), Int] = {
       if (apply(i) == apply(j) && cl == 2) m + ((i, j) -> 2)
       else if (apply(i) == apply(j)) && m((i + 1, j - 1)) == j - i - 1) m + ((i, j) -> (m(( i + 1, j - 1)) + 2))
       else m + ((i, j) -> max(m((i + 1, j)), m((i, j - 1))))    
     }
     def loop(m: Map[(Int, Int), Int], cl: Int, i: Int): Int = {
       if (cl > length) m((0, length - 1))
       else if (i >= length - cl + 1) loop(m, cl + 1, 0)
       else loop(setM(m, cl, i, i + cl - 1), cl, i + 1)
     }
     def initialize(m: Map[(Int, Int), Int] i: Int, j: Int): Map[(Int, Int), Int] = {
       if (i >= length) m
       else if (j >= length) initialize(m, i + 1, 0)
       else if (i == j) initialize(m +((i, j) -> 1), i, j + 1)
       else initialize(m + ((i, j) -> 0), i, j + 1)
     }
     loop(initialize(Map,empty[(Int, Int), Int], 0, 0), 2, 0)         
   }
   
   def length: Int =
     if (isEmpty) 0
     else 1 + tail.length
   
   override def toString: String = {
       def loop(h: A, t: List[A], s: String): String =
         if (!t.isEmpty) loop(t.head, t.tail, s + h + ", ")
         else s + h
         
       if (isEmpty) "List[]"
       else "List[" + loop(head, tail, "") + "]"
     }

   def fail(m: String) = throw new NoSuchElementException(m)
}

case object Nil extends List[Nothing] {
  def head: Nothing = fail("an empty list.")
  def tail: List[Nothing] = fail("an empty list.")
  def isEmpty: Boolean = true
}

case class Cons[A](head: A, tail: List[A]) extends list[A] {
  def isEmpty: Boolean = false
}

object List {
  def empty[A]: List[A] = Nil
  def make[A](x: A, t: List[A] = Nil): List[A] = Cons(x, t)
  def apply[A](xs: A*): List[A] = {
    var r: List[A] = List.empty
    for (x <- xs.reverse) r = r.prepend(x)
    r
  }
}



   
