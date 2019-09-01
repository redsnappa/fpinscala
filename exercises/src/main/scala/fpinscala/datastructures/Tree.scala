package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  }

  def maximum(tree:Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree:Tree[A]):Int = tree match {

    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left) max depth(right)

  }

  def map[A](tree:Tree[A])(op: A => A): Tree[A] = tree match {
    case Leaf(value) => Leaf(op(value))
    case Branch(left, right) => Branch(map(left)(op), map(right)(op))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(value) => l(value)
    case Branch(left,right) => b(fold(left)(l)(b),fold(right)(l)(b))
  }

  def size2[A](tree:Tree[A]): Int = {
    fold(tree)((x:A) => 1)(1 + _ + _)
  }

  def maximum2(tree:Tree[Int]): Int = {
    fold(tree)((v:Int) => v)((x:Int,y:Int) => x max y)
  }

  def depth2[A](tree:Tree[A]): Int = {
    fold(tree)(_ => 1)(1 + _ max _)
  }

}