//https://github.com/vkostyukov/scalacaster/blob/master/src/collection/Stack.scala

class Stack[+A](self: List[A]) {
  def top: A = self.head
  def rest: Stack[A] = new Stack(self.tail)
  def isEmpty: Boolean = self.isEmpty
  def pop: (A, Stack[A]) = (top, rest)
  def push[B >: A](x: B): Stack[B] = new Stack(x :: self)
}

object Stack {
   def empty[A]: Stack[A] = new Stack(Nil)
   def apply[A](xs: A*): Stack[A] =
     xs.foldLeft(Stack.empty[A])((r, x) => r.push(x))
}