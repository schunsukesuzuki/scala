//http://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-reduce-example/

//As per the Scala documentation, the definition of the reduce method is as follows:

def reduce[A1 >: A](op: (A1, A1) ⇒ A1): A1


//https://qiita.com/544/items/f36c018d0568156d09cb#reducefold

scala> (1 to 10).toList.reduceLeft{ _ + _ }
res5: Int = 55

scala> (1 to 10).toList.foldLeft(1){ _ + _ }
res6: Int = 56


//https://gist.github.com/hanbzu/7247300

// Reduction: We can sum all the elements
// with the usual recursive schema

def sum(xs: List[Int]): Int = xs match {
  case Nil     => 0
  case y :: ys => y + sum(ys)
}

// But this can be abstracted out using 'reduceLeft'
// That will perform these operations LINKING to the left

def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) = x + y)
def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) = x * y)

// A shorter way
def sum(xs: List[Int]) = (0 :: xs) reduceLeft ( _ + _ )
def product(xs: List[Int]) = (1 :: xs) reduceLeft ( _ * _ )

// A more general function: foldLeft
def sum(xs: List[Int]) = (xs foldLeft 0) ( _ + _ )
def product(xs: List[Int]) = (xs foldLeft 1) ( _ * _ )

// Other ways to write that
def sum(xs: List[Int]) = xs.foldLeft(0)( _ + _ )
def product(xs: List[Int]) = xs.foldLeft(1)( _ * _ )

//infix関数とprefix関数
//prefix関数とは，関数を先頭に引数をその後に並べて呼び出すような関数のことを指します．通常の関数がその例です．一方，infix関数とは，関数を先頭に引数をその後ろに並べて呼び出すような関数を指します．演算子がその例です．ちなみにprefixは接頭辞，infixとは接中辞という意味です．

// scala operator infix (if operator ends in : reverse order)
// Note: /: is alternate syntax for foldLeft; z /: xs is the same as xs foldLeft z.
def sum(xs: List[Int]) = (0 /: xs) ( _ + _ )
def product(xs: List[Int]) = (0 /:) ( _ * _ )


// How would we implement reduceLeft and foldLeft in List?

abstract class List[T] { ...
  def reduceLeft(op: (T, T) => T): T = this match {
    case Nil     => throw new Errot("Nil.reduceLeft")
    case x :: xs => (xs foldLeft x)(op)
    case x :: xs => (xs foldLeft x)(op)
  }
  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
    case Nil     => z
    case x :: xs => (xs foldLeft op(z, x))(op)
  }
}

// There's also reduceRight and foldRight.
// They produce the same results but sometimes only one
// of them is appropiate

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) ( _ :: _)

// This is what happens with foldRight:
//   ::
// x1  ::
//   x2  ::
//     xn  ::
//       y1  ::
//         y2  ::
//           yn  Nil
// BUT: foldLeft would not work here because
// :: is not applicable to elements, only to lists
// (try rebuilding the tree above for foldLeft)
// Or visualising it with operator infix

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (ys :\ xs) ( _ :: _ )

// ** foldRight and foldLeft ***
// Northern wind comes from the North (Richard Bird)
List(a, b, c).foldRight(e)(f) // equals...
f(a, f(b, f(c, e))) // <----

List(a, b, c).foldLeft(e)(f) // equals...
f(f(f(e, a), b), c) // ---->