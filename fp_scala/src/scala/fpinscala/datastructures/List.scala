package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](list: List[A]): A = list match {
    case Cons(x, _) => x
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def setHead[A](list: List[A], newHead: A): List[A] = {
    Cons(newHead, tail(list))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l 
    else drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = 
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(l: List[Int]): Int = foldRight(l, 0)((x, y) => x + y)

  def product(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

  /**
    * foldRight replaces the constructors of the List. 
    * Nil is replaced by z and Cons is replaced by f.
    * To compute the length we replace Cons(x, y) with f(x, foldRight(xs, z)(f).
    * If we set f to 1 + y, we will add 1 for every element in the list and finally 0 for Nil.
    *
    * foldRight(Cons(1, Cons(2, Cons(3, Nil))))
    * 1 + foldRight(Cons(2, Cons(3, Nil)))
    * 1 + 1 + foldRight(Cons(3, Nil))
    * 1 + 1 + 1 + foldRight(Nil)
    * 1 + 1 + 1 + 0 = 3
    *
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
}
