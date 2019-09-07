

object combinitorialProblems {

  val n = 7
  (1 until n) flatMap( i =>
    (1 until i).map( j => (i,j)))



  def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
    (for {
      (x,y) <- xs zip ys
    } yield x * y).sum
  }
}