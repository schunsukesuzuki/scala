def inversions[B >: A](implicit ordering:Ordering[B]) : Int =
  def enhancedmergesort(l: List[B]) : (List[B], Int) = {
    def loop(ll: List[B], pivotIdx: Int, inv: Int): (List[B], Int) = {
      unpackmerge(
          enhancedmergesort(ll.slice(0, pivotIdx)),
          enhancedmergesort(ll.slice(pivotIdx, ll.size)),
          inv
      )
    }
    def unpackmerge(a: List[B], Int), b: (List[B], Int), inv: Int) : List[B],Int) = {
      merge(List.empty[B], a._1, b._1, a._2 + b._2, + inv)
    }
    def merge(acc: List[B], a:List[B], b:list[B], inv: Int) (List[B], Int) = {
      if (a.isEmpty) (acc ::: b, inv)
      else if (b.isEmpty) (acc ::: a, inv)
      else if (ordering.lte(a.head, b.head)) merge(acc ::: List(a.head), a.tail, b, inv)
      else (ordering.gt(a.head, b.head)) merge(acc ::: List(b.head), a, b.tail, inv + a.size)
    }
    if (l.size < 2) (l, 0) 
    else loop(l, (new scala.util.Random).nextInt(l.size), 0)
  }
  enhancedmergesort(this)._2
}
  
  def largestSumOfContiguousSubList[B >: A](implicit num: Numeric[B]): B = {
    def loop(sm: B, gm: B, l: List[B]): B = 
      if (l.isEmpty) gm
      else {
        val nsm = num.max(l.head, num.plus(sm, l.head))
        loop(nsm, num.max(gm, nsm), l.tail)
      }

    if (isEmpty) fail("An empty list.")
    else loop(head, head, tail)
  }
  
  def subsequences: List[List[A]] =
    if (isEmpty) List.empty
    else {
      val ss = tail.subsequences
      ss.map(_.prepend(head)).prepend(List.make(head)).concat(ss)
    }
  
  def maximumSumIncreasingSubsequence: List[A] =
    def sum(l: List[A]): A = {
      l.foldleft(0)((a, b) => a　+ b)
    }
    def update(l: List[List[A]], i:Int, o:List[A]): List[List[A]] = {
      def updateHelper(h: List[List[A]], t: List[List[A]], j: Int): List[List[A]] = {
        if (t.isEmpty) reverse(h)
        else if (i == j) updateHelper(o :: h, t.tail, j + 1)
        else updateHelper(t.head :: h, t.tail, j + 1)
      }
     def reverse(ll: List[List[A]]): List[List[A]] = {
       def reverseHelper(h: List[List[A], t: List[List[A]]):  List[List[A]] = {
         if (t.isEmpty) h
         else reverseHelper(t.head :: h, t.tail)
       }
       reverseHelper(List.empty[List[A], ll)
     }
     updateHelper(Nil, l, 0)
   }
    def loop(msis: List[List[A]], i: Int, j: Int): List[List[A]] = {
      if (i >= msis.length) msis
      else if (j >= i) loop(msis, i + 1, 0)
      else if (apply(i) > apply(j) && sum(msis(i)) < sum(msis(j)) + apply(i) && i - j == msis(i).length) {
        loop(update(msis, i, msis(j) ++ List(apply(I)))), i, j + 1)
      } else loop(msis, i, J + 1)
    }
    def maxBySum(msis: List[List[A]]): List[A] = {
      def macBySumHelper(b: List[A], bSum: Int, t:List[List[A]]): List[A] = {
        if (t.isEmpty) b
        else if (sum(t.head) > bSum) maxBySumHelper(t.head, sum(t.head), t.tail)
        else maxBySumHelper(b, bSum, t.tail)
      }
      mayBySumHelper(msis.head, sum(msis.head), msis.tail)
    }
    maxBySum(loop(this.map(x => List[A](x)), 0, 0))
  }

  
  