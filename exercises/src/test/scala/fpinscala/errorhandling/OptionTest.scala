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

  test("Should get something when exists"){
    val v = 1
    val input:Option[Int] = Some(v)
    assert(input.getOrElse(66) == v)
  }

  test("Should get default when nothing can be got"){
    val defaultValue = 66
    val input:Option[Int] = None
    assert(input.getOrElse(defaultValue) == defaultValue)
  }


  test("should lift/wrap existing function to give back option"){

    val existingFunction = (age:Int,numberOfTickets:Int) => age * numberOfTickets
    val optionAge:Option[Int] = Some(10)
    val numberOfTickets:Option[Int] = Some(3)

    val result = Option.map2(optionAge, numberOfTickets)(existingFunction)
    val expected = Some(30)
    assert(result == expected)

  }

}
