package com.scalafpbook

import scala.annotation.tailrec

object GettingStarted {
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int = n match {
      case 0 => acc1
      case 1 => acc2
      case _ => go(n-1, acc2, acc1+acc2)
    }
    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(index: Int): Boolean = {
      if (index == as.length) true
      else if (index == 0 || ordered(as(index-1), as(index))) go(index+1)
      else false
    }

    go(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    x => y => f(x, y)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (x, y) => f(x)(y)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def f(b: String): String = b + 10

  def g(a: String): String = a.toLowerCase

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(2,4,6,8,10,12).reverse, (x: Int, y: Int) => y+2 == x))
    println(uncurry(curry((x: Int, y: Int) => x + y))(1, 2))
    println(compose(f, g)("LACHICA"))
  }
}

// cap
