package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def main(args: Array[String]): Unit = {
    println("hello")
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a + b
    val zero = 0
  }


  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    val zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    def zero = true 
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a && b
    val zero = true
  }

  def dual[A](m: Monoid[A]) = new Monoid[A]{
    def op(a: A, b: A) = m.op(b, a)
    val zero = m.zero
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = 
      (a, b) match {
        case (None, None) => None
        case (Some(x), _) => Some(x)
        case (_, Some(y)) => Some(y)
      }
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {  
    def op(f: A => A, g: A => A) = f compose g // f(g(x))
    val zero = (i:A) => i
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  // trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => (b => f(b, a)))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    // Split into half and then combine using monoid
    if (as.isEmpty) {
      m.zero
    } else if(as.length == 1) {
      f(as(0))
    } else {
      val (a,b) = as.splitAt(as.length/2)
      m.op(foldMapV(a, m)(f), foldMapV(b, m)(f))
    }
  }
  
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[Option[(Int, Int)]]] {
      def op(a: Option[Option[(Int, Int)]], b: Option[Option[(Int, Int)]]): Option[Option[(Int, Int)]] = (a, b) match {
        case (Some(Some((l1, r1))), Some(Some((l2, r2)))) if (r1 <= l2) => Some(Some((l1, r2)))
        case (Some(None), Some(Some((l, r)))) => Some(Some((l, r)))
        case (Some(Some((l, r))), Some(None)) => Some(Some((l, r)))
        case (None, _) | (_, None) | (_, _) => None
      }
      val zero = Some(None)
    }
    foldMapV(ints, m)(i => Some(Some((i, i)))).isDefined
  }
  
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

    /**
      * part("apple", 1, "") part ("dog", 1, "") -> part("apple", 3, "")
      * part("apple", 1, "dog") part("", 1, "cat") -> part("apple", 3, "cat")
      * part("apple", 1, "") part("", 1, "cat") -> part("apple", 2, "cat")
      */
  val wcMonoid: Monoid[WC] = new Monoid[WC]{
    def op(a: WC, b: WC): WC = {
      (a, b) match {
        case (Part(l1, wc1, ""), Part("", wc2, r2)) => Part(l1, wc1 + wc2, r2)
        case (Part(l1, wc1, _), Part(_, wc2, r2)) => Part(l1, wc1 + wc2 + 1, r2)
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Stub(c1), Part(l1, wc1, r1)) => Part(c1 + l1, wc1, r1)
        case (Part(l1, wc1, r1), Stub(c1)) => Part(l1, wc1, r1 + c1)
      }
    }
    val zero = Stub("")
  }
  /**
    * "" -> stub("") -> 0
    * " a" -> " ", "a" -> Part("", 0, "") Stub("a") -> Part("", 0, "a") -> 1
    * "a b c" -> Part("a", 1, "c") -> 3
    * "a" -> stub("a") -> 1
    * "a  b" ->
    * "a b" -> "a", " ", "b"
    */
  def count(s: String): Int = {
    foldMapV(s.toIndexedSeq, wcMonoid)((char) => {// stub(_)
        if(char.isWhitespace) {
          Part("", 0, "")
        } else { 
          Stub(char.toString)
        }
    }) match {
      case Stub("") =>  0
      case Stub(word) => 1
      case Part(l, wc, r) => (if (l.isEmpty) 0 else 1) + wc + (if (r.isEmpty) 0 else 1)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}

