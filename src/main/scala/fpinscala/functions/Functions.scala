package fpinscala.functions

object Functions
{
  def factorial(n: Int): Int =
  {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = n match {
      case 0 => prev
      case 1 => cur
      case _ => loop(n - 1, cur, prev + cur)
    }
    loop(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) =
  {  val msg = "The %s of %d is %d."
    msg.format(n, f(n))
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }


  def main(args: Array[String]): Unit = {
    println(fib(5))
    println(isSorted(Array(1, 2 ,3),  (e:Int, b: Int) => e<b))
  }

}

