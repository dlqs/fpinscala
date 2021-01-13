package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    def isDone = a.isDone && b.isDone
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) && b.cancel(evenIfRunning)
    def get(timeout: Long, units: TimeUnit): A = {
      val start = System.currentTimeMillis()
      val ar = a.get(timeout, units)
      val br = b.get(TimeUnit.MILLISECONDS.convert(timeout, units) - (System.currentTimeMillis() - start), TimeUnit.MILLISECONDS)
      f(ar, br)
    }
  }

  def asyncF[A, B](f: A => B): A => Par[B] = {
    // ParB is executable service which returns lazy future
    (a) => lazyUnit(f(a))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      // Units 
      asyncF(filter(f))(as)
  }
  
  // map2(fork(a), fork(b))
  // map2(a, b)
  // TimedFuture[A]
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord 
  // with our design choice of having `fork` be the sole function in the API for controlling parallelism
  //. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to 
  // occur in a separate thread.
    //a.1 : 10
    //a.2 : 10
    //a: 10
    //b: 30
    // a +b : 40
    //a.1(timeout) + a.2(timeout) + b(timeout) = 50s
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      Map2Future(af, bf, f)
      // This implementation of `map2` does _not_ respect timeouts, 
      // and eagerly waits for the returned futures. This means that even if you have passed 
      // in "forked" arguments, using this map2 on them will make them wait. 
      // It simply passes the `ExecutorService` on to both `Par` values, waits for the results 
      // of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. 
      // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time 
      // spent evaluating `af`, then subtracts that time from the available time allocated for 
      // evaluating `bf`.
    }


  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but 
  // there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" 
  // task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource 
  // backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. 
  // Essentially, we're using two threads when one should suffice. This is a symptom of a more 
  // serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
