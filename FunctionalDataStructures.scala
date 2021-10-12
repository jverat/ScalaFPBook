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

  // exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }

  // exercise 3.11
  def sumFoldLeft(list: List[Int]): Int = {
    foldLeft(list, 0)((n1: Int, n2: Int) => n1 + n2)
  }

  // exercise 3.11
  def product(list: List[Double]) = {
    if list.contains(0.0) then
      0.0
    else
      foldLeft(list, 1.0)((n1: Double, n2: Double) => n1 * n2)

  }

  // exercise 3.11
  def lengthLeft[A](list: List[A]) = {
    foldLeft(list, 0)((x, y) => 1 + x)
  }

  // exercise 3.12
  def reverse[A](list: List[A]) = {
    foldLeft(list, List[A]())((x, y) => y :: x)
  }

  // exercise 3.13
  def foldRight2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, z)(f)
    }
  }


  def sum3(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    if ns.contains(0.0) then
      0.0
    else
      foldRight(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => 1 + y)

  // exercise 3.14
  /*def append[A](as: List[A], a: A): List[A] = {
    foldLeft(as, List(a))((x: List[A], y: List[A]) => {
      x :: y
    })
  }*/
  @main def main(): Unit = {
    //println(append((List(1, 2, 3, 4, 5)), 6))
  }
}

//pag 62