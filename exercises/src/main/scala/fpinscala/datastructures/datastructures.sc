sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ints: List[Int]): Int = ints match {

    case Nil => 1
    case Cons(x,xs) => x * product(xs)

  }

  def reduceLeft[A](ints:List[A])(op: (A,A) => A): A = ints match {
    case Nil => throw new Error("Can't reduceLeft List of type Nil")
    case Cons(x,xs) => foldLeft(xs, x)(op)
  }

  def foldLeft[A,B](ints:List[A],z:B)(op:(B,A) => B): B = ints match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, op(z,x))(op)
  }

  def sum2(ints:List[Int]):Int = foldLeft(ints, 0)(_ + _)

  def product2(ints:List[Int]):Int = foldLeft(ints, 1)(_ * _)


  def reduceRight[A](ints:List[A], op: (A, A) => A): A = ints match {
    case Nil => throw new Error("Can't reduceRight of Nil")
    case Cons(x,Nil) => x
    case Cons(x,xs) => foldRight(xs, x)(op)
  }

  def foldRight[A,B](ints:List[A], z:B)(op: (A,B)=> B): B = ints match {

    case Nil => z
    case Cons(x,xs) => op(x, foldRight(xs,z)(op))
  }


}


List.sum(Cons(1,Cons(2,Cons(3, Nil))))

List.product(Cons(1,Cons(2,Cons(3, Cons(4,Nil)))))

List.sum2(Cons(1,Cons(2,Cons(3, Nil))))

List.product2(Cons(1,Cons(2,Cons(3, Cons(4,Nil)))))