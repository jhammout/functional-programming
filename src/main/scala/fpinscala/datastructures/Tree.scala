package fpinscala.datastructures



sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  /*def maximum(t: Tree[Int]): Int = t match {
    case Leaf(_) => _
    case Branch(left, right) => maximum(left) max maximum(right)
  }*/

  def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => depth(left) max depth(right)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(e) => Leaf(f(e))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _+_)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(e => e)(_ max _)

  def depthViaFold[A](t: Tree[A]) : Int = fold(t)(e => 1)(1 + _ max _)
}