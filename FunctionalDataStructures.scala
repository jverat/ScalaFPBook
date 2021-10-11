package com.scalafpbook

import scala.annotation.tailrec

object FunctionalDataStructures {
  // exercise 3.3
  def setHead[A](list: List[A], newHead: A): List[A] = {
    newHead :: list.tail
  }

  // exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0)
      l
    else
      drop(l.tail, n - 1)
  }

  // exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    if (f(l.head))
      dropWhile(l.tail, f)
    else
      l
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case _ :: Nil => List()
      case _ => l.head +: init(l.tail)
    }
  }


  // exercise 3.7
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    if ns.contains(0.0) then
      0.0
    else
      foldRight(ns, 1.0)(_ * _)

  // exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }

  @main def main(): Unit = {
  }

}

//pag 56