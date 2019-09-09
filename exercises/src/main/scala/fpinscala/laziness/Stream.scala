package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def toList(): List[A] = {

    @tailrec
    def inner(in: Stream[A], result: List[A]): List[A] = in match {
        case Empty => result
        case Cons(x, xs) => inner(xs(), x() :: result)
    }

    inner(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ if n == 0 => Empty
    case Cons(h,t) => cons(h(), t().take(n - 1))

  }

  def drop(n: Int): Stream[A] = this match {

    case Cons(h, t) if n <= 0 => this
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => empty

  }

  def takeWhile(p: A => Boolean): Stream[A] = {

    foldRight(empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else empty )

  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a,b) => cons(f(a),b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if(f(a)) cons(a,b) else b)
  }

  def append[B>:A](value: =>Stream[B]): Stream[B] = {
    foldRight(value)((h,t) => cons(h,t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B]={
    foldRight(empty[B])((h,t) => f(h) append t)
  }



  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constants[A](value: A): Stream[A] = {
    cons(value, constants(value))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(n: Int): Stream[Int] = {

    def inner(count:Int, stream: Stream[Int]): Stream[Int] = {

      if(count == 0){
        cons(0, empty)
      }else if(count == 1) {
        cons(1, inner(count -1, stream))
      }else{
        cons( )
      }

    }

    inner(n, empty[Int])

  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}