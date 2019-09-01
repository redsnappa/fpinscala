import scala.annotation.tailrec

val arrayLiteral = Array(1,23,45)

val functionLiteral = (x:Int, y:Int) => x == y


def poly[A](array:Array[A], x:A) : Boolean = {

  @tailrec
  def loop(array:Array[A], index:Int): Boolean ={

    if(index >= array.length )
      return false
    else if(array(index) == x)
      return true
    else
      loop(array, index + 1)
  }
  loop(array,0)
}


poly(Array(1,2,3,4,5), 3)
poly(Array(1,2,3,4,5), 8)

poly(Array('a','b','c','d','e'), 'd')
poly(Array('a','b','c','d','e'), 'y')
