package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(f.curried))(fa))(fb)
  }

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc:F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc:F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =  {
    traverse(fas)(a => a)
  }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = 
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
    map2(fa, fb)((a,b) => (a,b))
  }

  def product[G[_]](g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    type H[A] = (F[A], G[A])
    val self = this
    new Applicative[H]{
      def unit[A](a: => A) = (self.unit(a), g.unit(a))
      override def map2[A, B, C](ha: H[A], hb: H[B])(f: (A, B) => C) = {
        val (fa, ga) = ha
        val (fb, gb) = hb
        (self.map2(fa, fb)(f), g.map2(ga, gb)(f))
      }
    }
  }

  def compose[G[_]](g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    type H[A] = F[G[A]]
    val f = this

    new Applicative[H] {
      def unit[A](a: => A) = f.unit(g.unit(a))
      override def apply[A, B](hab: H[A => B])(ha: H[A]): H[B] = {
        f.map2(hab, ha)((gab, ga) => g.apply(gab)(ga))
        // f.apply(f.apply(f.unit(gab => ga => g.apply(gab)(ga)))(hab))(ha)
      }
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map[K, V]())){ case (acc, (k, fv)) =>
      map2(acc, fv)((acc: Map[K,V], v: V) => acc + ((k, v)))
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))


}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](either: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      either.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = {
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def map2[A, B, C](a: Validation[E, A], b: Validation[E,B])(f: (A, B) => C): Validation[E, C] = {
      (a, b) match {
        case (Success(c), Success(d)) => Success(f(c,d))
        case (Failure(head, tail), Failure(head2, tail2)) => Failure(head, (head2 +: tail) ++ tail2)
        case (Success(_), Failure(h, t)) => Failure(h, t)
        case (Failure(h, t), Success(_)) => Failure(h, t)
      }
    }
    def unit[A](a : => A):Validation[E, A]  = Success(a)
  }
  }

  implicit val optionApplicative = new Applicative[Some] {
    def unit[A](a: => A): Some[A] = Some(a)
    override def map2[A, B, C](fa: Some[A], fb: Some[B])(f: (A, B) => C): Some[C] = {
      (fa, fb) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        //case (_, _) => None
      }
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  import Applicative._

  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    traverse(fa)(a => Some(f(a))) match {
      case Some(fb) => fb
      //case None => throw new Exception()
    }
  }




  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =  {

    traverseS(fa)((a: A) => (for {
      as <- get[List[A]]
      _ <-set(a::as)
    } yield ()).run(Nil)._2

  }

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List]{
    override def traverse[G[_], A, B](la: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      G.sequence(la.map(f))
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = {
      oa match {
        case Some(x) => G.map(f(x))(x => Some(x))
        case None => G.unit(None)
      }
    }
  }
// G[List[Tree[B]]]
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = ta match {
      case Tree(head, tail) => G.map2(f(head), listTraverse.traverse(tail)(traverse(_)(f)))(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
