package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import language.postfixOps
import language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = 
  Prop(
    (tc, rng) => (run(tc, rng), p.run(tc, rng)) match {
      case (Passed, Passed) => Passed
      case (Passed, Falsified(f, s)) => Falsified(f, s)
      case (Falsified(f, s), Passed) => Falsified(f, s)
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1 + f2, s1 + s2)
    })
  def ||(p: Prop): Prop = Prop(
    (tc, rng) => (run(tc, rng), p.run(tc, rng)) match {
      case (Passed, _) | (_, Passed) => Passed
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(f1 + f2, s1 + s2)
    })
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find((r: Result) => r.isFalsified).getOrElse(Passed)
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    randToGen(RNG.range(start, stopExclusive))
  }

  def unit[A](a: => A): Gen[A] =  {
    randToGen(RNG.unit(a))
  }

  def boolean: Gen[Boolean] = {
    choose(0, 2).map((x: Int) => x == 0)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    if (n <= 0) return unit(List())
    g.map2(listOfN(n - 1, g))((a, acc) => a::acc)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }
  def randToGen[A](r: RNG.Rand[A]): Gen[A] = {
    Gen(State(r))
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    randToGen(RNG.double1).flatMap((b) => if (b <= ((g1._2)/(g1._2 + g2._2))) g1._1 else g2._1)
  }
}
  
case class Gen[A] (sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =  {
    Gen( 
      sample.map(f)
    )
  }

  def map2[B, C](genB: Gen[B])(f: (A, B) => C) = {
    flatMap(a => genB.map(b => f(a, b)))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(
      sample.flatMap(a => f(a).sample)
    )
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }
}
