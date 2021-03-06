package fpinscala.localeffects

import fpinscala.monads._

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}
/*
val p = new RunnableST[(Int, Int) /* A */] { 
  def apply[S] = for { // ST[S,A]
    r1 <- STRef(1) // ST[S, STRef[S, Int]], r1 = STRef[S, Int]
    r2 <- STRef(2)
    x  <- r1.read  // ST[S, Int] x = Int
    y <- r2.read  
    _ <- r1.write(y+1)
    _ <- r2.write(x+1)
    _ <- r1.read  
    b <- r2.read
  } yield (a,b)
}
// p.apply[S]()
*/

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S,A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S,List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    // xs.toList.foldLeft(ST(()))((s, kv) => k match { case (i, a) => s.flatMap(write(i,a)) })
    for ((i, a) <- xs) {
      write(i, a)
    }
    ST(())
  }
  
  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })
}

object Immutable {
  def noop[S] = ST[S,Unit](())

  def partition[S](arr: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = {
    // def partition_imperatively(arr: Array[Int], l: Int, r: Int, pivot: Int): ST[S, Int] = {
    //   val pivot_value = arr(pivot)
    //   var part_index = l
    //   arr(l) = arr(pivot)
    //   arr(pivot) = pivot_value
    //   for (i <- l to r) {
    //     val val_i = arr(i)
    
    // //_ <- (l until r).foldLeft(noop[S])((s, i) => for {
    //     if (val_i < pivot_value) {
    //       var temp = arr(i) 
    //       arr(i) = arr(part_index)
    //       arr(part_index) = temp
    //       part_index += 1
    //     }
    //   }
    //   arr(l) = arr(part_index)
    //   arr(part_index) = pivot_value 
 
    //   ST(part_index)
    // }
 
    for {
      pivot_value <- arr.read(pivot)
      part_index <- STRef(l)
      _ <- arr.swap(l, pivot)
      _ <- (l until r).foldLeft(noop[S])((s, i) => for {
        val_i <- arr.read(i)
        _ <- {
          if (val_i >= pivot_value) noop[S]
          else
          for {
            idx <- part_index.read
            _ <- arr.swap(i, idx)
            _ <- part_index.write(idx + 1)
          } yield ()
        }
      } yield ())
      new_part_index <- part_index.read
      _ <- arr.swap(l, new_part_index)
    } yield new_part_index
  }

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] = {
    for {
      p <- partition(a, l, r, (l + r)/ 2)
      _ <- qs(a, l, p -1)
      _ <- qs(a, p + 1, r)
    } yield ()
  }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
  })
}

import scala.collection.mutable.HashMap

sealed trait STMap[S, K, V] {
  protected var hmap: HashMap[K, V]
  def size: ST[S,Int] = ST(hmap.size)

  // Write a value at the give index of the array
  def put(k: K, v: V): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      hmap.put(k, v)
      ((), s)
    }
  }

  def get(k: K): ST[S,V] = ST(hmap(k))
}

object STMap {
  def empty[S, K,V]: ST[S, STMap[S,K,V]] = 
    ST(new STMap[S,K,V] { 
      var hmap = new HashMap[K, V]() 
    })

  def fromAssocList[S, K,V](assocList: List[(K,V)]): ST[S, STMap[S,K,V]] =
    assocList.foldLeft(empty[S,K,V])((m, kv) => kv match {
      case (k, v) => 
        for {
          map <- m
          _ <- map.put(k,v)
        } yield map
    })
}