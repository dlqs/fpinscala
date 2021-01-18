package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  type FailedCase = String
  type SuccessCount = Int
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.range(start, stopExclusive)))
  }
  def unit[A](a: => A): Gen[A] =  {
    Gen(State(RNG.unit(a)))
  }
  def boolean: Gen[Boolean] = {
    choose(0, 2).map((x: Int) => x == 0)
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    if (n <= 0) return unit(List())
    g.map2(listOfN(n - 1, g))((a, acc) => a::acc)
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

//case class Gen[A] extends Gen[A](sample: State[RNG, A])

trait SGen[+A] {

}

