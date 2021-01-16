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
    def helper(count: Int, rng: RNG, acc: List[A]): (List[A], RNG)= {
      if (count <= 0) return (acc, rng)
      val (i, rng2) = g.sample.run(rng)
      helper(count - 1, rng2, i::acc)
    }
    Gen(State((rng) => helper(n, rng ,List())))
  }

}


case class Gen[A] (sample: State[RNG, A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

//case class Gen[A] extends Gen[A](sample: State[RNG, A])

trait SGen[+A] {

}

