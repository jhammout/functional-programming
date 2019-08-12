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

  def foldRight[A,B](l:List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](l:List[A], z: B)(f: (B,A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]) : List[A] = foldLeft(l, List[A]())((tail, h) => Cons(h, tail))

  def foldRightViaFoldLeft[A, B] (l: List[A], z: B ) (f: (A,B) => B ) :  B = foldLeft(reverse(l), z)((b,a)=> f(a,b))

  def foldRightViaFoldLeft_2[A, B] (l: List[A], z: B ) (f: (A,B) => B ) :  B = foldLeft(l, (b:B) => b)((g,a)=> b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A, B] (l: List[A], z: B ) (f: (B,A) => B ) :  B = foldRight(l, (b:B) => b)((a,g)=> b => g(f(b,a)))(z)

  def lengthFunctional[A](ls: List[A]): Int = foldLeft(ls, 0) { (c, _) => c + 1 }

  def isPalindrome[A](l: List[A]) : Boolean = l == reverse(l)

  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, Nil:List[B])((h, tail) => Cons(f(h), tail))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def flatten[A](ls: List[A]): List[A] = flatMap(ls) {
    case ms: List[A] => flatten(ms)
    case e => List(e)
  }

  def compress[A](l: List[A]): List[A] = foldRight(l, List[A]())((h, tail) => if (head(tail) != h) Cons(h, tail) else tail)

  def apply[A](as: A*) : List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))



  def main(args: Array[String]): Unit = {
    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1,2,3,10,2,6,6,5,8,9,7,4)
    val total = sum(example)
    println(head(example2))
  }

}
