package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def main(args: Array[String]): Unit = {
    println(tail(List(1, 2, 3)))
    println(drop(List(1, 2, 3), 2))
    println(dropWhile(List(2, 2, 3, 4), (x: Int) => x == 2))
    println(init(List(1, 2, 3)))
    println(length(List(1, 2, 3)))
    println(foldLeft(List(1, 2, 3, 4), 1)(_ * _))
    println(sum3(List(1, 2, 3, 4)))
    println(product3(List(1, 2, 3, 4)))
    println(length2(List(1, 2, 3)))
    println(reverse(List(1, 2, 3, 4)))
    println(append2(List(1, 2), List(3, 4)))
    println((concat(List(List(1, 2), List(3, 4)))))
    println(add1(List(1, 2, 3, 4)))
    println(doubleToString(List(1.0, 2.0, 3.0)))
    println(map(List(1, 2, 3, 4))((x) => x + 1))
    println(filter(List(1, 2, 3, 4))((x) => x % 2 == 0))
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(filter2(List(1, 2, 3, 4))((x) => x % 2 == 0))
    println(addPairwise(List(1, 2), List(3, 4)))
    println(zipWith(List(1, 2), List(3, 4))((a, b) => a + b))
  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("empty list")
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("gave head to empty list")
      case Cons(_, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) return l
    drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, len) => len + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Int]) = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((h, acc) => Cons(h, acc))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, List[A]())(append)
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((h, acc) => Cons(h + 1, acc))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, List[String]())((h, acc) => Cons(h.toString, acc))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((h, acc) => Cons(f(h), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((h, acc) => append(f(h), acc))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a) => if (f(a)) List(a) else Nil)
  }

  def addPairwise[A](l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (_, Nil) => l1
      case (Nil, _) => l2
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    (l1, l2) match {
      case (_, Nil) => l1
      case (Nil, _) => l2
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }
}
