package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A] (head:A, tail:List[A]) extends List[A]

object List {

  def sum(ints:List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints:List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def last[A](l:List[A]): A = l match {
    case Nil => throw new NoSuchElementException
    case Cons(x ,Nil) => x
    case Cons(_ , xs) => last(xs)
  }

  def penultimate[A](l:List[A]): A = l match {
    case Cons(x1, Cons(_ ,Nil)) => x1
    case Cons(_ , xs) => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  def head[A](l:List[A]): A = l match {
    case Cons(h , _) => h
    case Nil=> throw new NoSuchElementException
  }

  def tail[A](l:List[A]): List[A] = l match {
    case Cons(_ , xs) => xs
    case Nil=> throw new NoSuchElementException
  }



  def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    @tailrec
     def lastNthR[A](count: Int, resultList: List[A], curList: List[A]): A =
    curList match {
      case Nil if count > 0 => throw new NoSuchElementException
      case Nil              => head(resultList)
      case Cons(_, tail) =>
        lastNthR(count - 1,
          if (count > 0) resultList else List.tail(resultList),
          tail)


    }

    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }

  def length[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(_, tail) => 1 + length(tail)
    }

  def lengthR[A](l: List[A]): Int = {
    def len[A](curList: List[A], acc: Int): Int =
      curList match {
        case Nil => acc
        case Cons(_, tail) => len(tail, acc + 1)
      }

    len(l, 0)
  }

  //def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 }




  def apply[A](as: A*) : List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))



  def main(args: Array[String]): Unit = {
    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1,2,3,10,2,6,5,8,9,7,4)
    val total = sum(example)
    println(lengthR(example2))
  }

}
