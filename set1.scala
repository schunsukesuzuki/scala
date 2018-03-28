//https://github.com/vkostyukov/scalacaster/blob/master/src/collection/Set.scala

abstract class Set[+A] {
  def isEmpty: Boolean = ???
  def size: Int = ???
  def contains[B >: A](x: B): Boolean = ???
  def union[B >: A](s: Set[B]): Set[B] = ???
  def intersection[B >: A](s: Set[B]): Set[B] = ???
  def difference[B >: A](s: Set[B]) = ???
  def isSubset[B >: A](s: Set[B]): Boolean = ???
  def combinations(n: Int): List[Set[A]] =
    if (k > size) Nil // Set()
    else if (k == 1) map(Set(_)).toList
    else tail.combinations(k - 1).map(_ + head) :: tail.combinations(k)
  def subsets: List[Set[A]] =
    if (isEmpty) Nil
    else (2 to size).foldLect(s.combinatinos(1))((a, i) => s.combinations(i) :: a)
  def maxSumSubset: Set[A] = ???
  def maxSumSubset(n: Int): Set[A] = ???
  }
}
