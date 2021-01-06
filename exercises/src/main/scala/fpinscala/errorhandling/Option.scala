package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case None => default
      case Some(x) => x
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(x) => f(x)
    }
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(x) => Some(x)
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(x) if f(x) => Some(x)
      case _ => None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def main(args: Array[String]): Unit = {
    val s: Option[Int] = Some(2)
    // [Int] is needed to pin the type down
    val t: Option[Int] = None
    println(s.map(_ + 1))
    println(t.map(_ + 1))
    println(s.filter(_ % 2 != 0))
    println(t.filter(_ % 2 != 0))
    println(s.flatMap((x) => if (x != 0) Some(8 / x) else None)) // check that we don't divide by zero
    println(t.flatMap((x) => if (x != 0) Some(8 / x) else None))
    println(s.orElse(Some(4)))
    println(t.orElse(Some(4)))
    println(s.getOrElse(4))
    println(t.getOrElse(4))
    println(variance(Seq(600, 470, 170, 430, 300))) // 21704
    println(map2(None, Some(2))((a: Int, b: Int) => a + b))
    println(map2(Some(3), Some(2))((a: Int, b: Int) => a + b))
  }
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap ((m) => mean(xs map ((x) => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case(Some(s), Some(t)) => Some(f(s, t))
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}