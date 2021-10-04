package com.scalafpbook

import scala.annotation.tailrec

object FunctionalDataStructures {
  def setHead[A](list: List[A], newHead: A): List[A] = {
    newHead :: list.tail
  }

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

  @main def main(): Unit = {
    val myList = List(1, 2, 3, 4, 5)
    println(init(myList))
    val myList2 = List("uno", "doce", "trece")
    println(init(myList2))
  }

}

//pag 56