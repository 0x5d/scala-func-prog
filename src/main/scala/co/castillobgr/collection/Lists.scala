package co.castillobgr.collection

import java.util.NoSuchElementException

object Lists {

  // Implemented in terms of foldLeft for tail-recursiveness
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    if (as == Nil) z
    else {
      def loop(as: List[A], curr: B): B =
        if (as == Nil) curr
        else loop(tail(as), f(curr, head(as)))
      loop(as, z)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => l
      case _ :: xs => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case x :: xs if (f(x))=> dropWhile(xs, f)
    case _ => l
  }

  def head[A](as: List[A]): A = as match {
    case Nil => throw new NoSuchElementException("Head of empty list")
    case x :: xs => x
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case List(_) => Nil
    case _ :: xs => xs
    case _ :: xs => xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case List(x, y) => List(x)
    case x :: xs => x :: init(xs)
  }

  def flatten[A](as: List[List[A]]): List[A] =
    foldLeft(as, List[A]())(_ ++ _)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, List[B]())((bs, a) => bs ++ List(f(a)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, List[A]())((filtered, next) => {
      if(f(next)) filtered ++ List(next)
      else filtered
    })
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => List(h)
    case List(_) => List(h)
    case _ :: xs => h :: xs
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  def sum(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int =
    foldLeft(as, 0)(_ * _)

  def reverse[A](as: List[A]): List[A] = as match {
    case Nil => as
    case List(x) => as
    case _ :: _ => foldLeft(as, List[A]())((xs, x) => x :: xs)
  }

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = as match {
    case List() => true
    case List(_) => true
    case List(_, _*) => {
      def loop(it: Iterator[List[A]]): Boolean = {
        if (!it.hasNext) true
        else {
          val elems = it.next()
          if (!ordered(elems.head, elems.last)) false
          else loop(it)
        }
      }
      loop(as.sliding(2))
    }
  }
}
