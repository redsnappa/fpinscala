package fpinscala.errorhandling

import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  test("Should map Option"){

    val input:Option[Int] = Some(5)

    assert( input.map(_ + 1) == Some(6))

  }

  test("Should map None"){

    val input:Option[Int] = None
    assert(input.map(_ + 1) == None)
  }

}
