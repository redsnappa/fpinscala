package fpinscala.errorhandling

import org.scalatest.{FunSuite, Matchers}



class EitherTest extends FunSuite with Matchers {

  test("Should handle age failure with Either"){
    val result:Either[Exception, Double] = parseInsurance("text", "10")

    assert(result.isInstanceOf[Left[Exception]])
  }

  test("Should handle ticket failure with Either"){
    val result:Either[Exception, Double] = parseInsurance("10", "test")

    assert(result.isInstanceOf[Left[Exception]])
  }

  test("Should process valid input"){
    val result:Either[Exception, Double] = parseInsurance("10", "10")

    assert(result.isInstanceOf[Right[Double]])
  }


  def parseInsurance(age:String, numberOfSpeedingTickets:String):Either[Exception, Double] ={
    for {
      a <- Either.Try(age.toInt)
      tickets <- Either.Try(numberOfSpeedingTickets.toInt)
    } yield Either.insuranceRateQuote(a,tickets)
  }
}
