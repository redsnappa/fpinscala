package fpinscala.laziness

import org.scalatest.FunSpec

class StreamTest extends FunSpec {

  describe("A Stream") {

    it("should convert to a List") {
      val streamOfInts = Stream[Int](1, 2, 3, 4, 5)
      val result: List[Int] = streamOfInts.toList()
      assert(result == List(1, 2, 3, 4, 5))
    }

    it("should take first n elements") {

      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Stream[Int] = streamOfInts.take(5)

      assert(result.toList() == Stream[Int](1, 2, 3, 4, 5).toList())

    }

    it("should drop first n elements") {

      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Stream[Int] = streamOfInts.drop(5)

      assert(result.toList() == Stream[Int](6, 7, 8, 9, 10).toList())

    }

    it("should take elements until it does not match a given predicate") {

      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Stream[Int] = streamOfInts.takeWhile(_ < 7)

      assert(result.toList() == Stream[Int](1, 2, 3, 4, 5, 6).toList())

    }

    it("should hold true for all elements in the stream") {

      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Boolean = streamOfInts.forAll(i => i > 0)

      assert(result)

    }

    it("should exit early when stream forall does not hold true") {

      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Boolean = streamOfInts.forAll(i => i < 8)

      assert(!result)

    }

    it("should map a stream to its arguments") {
      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Stream[Int] = streamOfInts.map(i => i * 2)

      assert(result.toList() == List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))


    }

    it("should filter against a predicate") {
      val streamOfInts = Stream[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val result: Stream[Int] = streamOfInts.filter(_ % 2 == 0)

      assert(result.toList() == List(2, 4, 6, 8, 10))
    }

    it("should append value at the end") {
      val streamOfInts = Stream[Int](1, 2, 3)
      val secondStreamOfInts = Stream[Int](4, 5, 6)
      val result: Stream[Int] = streamOfInts.append(secondStreamOfInts)

      assert(result.toList() == List(1, 2, 3, 4, 5, 6))
    }

    it("should flatmap a stream") {

      val streamOfInts = Stream[Int](1, 2, 3)

      val result: Stream[Int] = streamOfInts.flatMap((i: Int) => streamOfInts.take(i))

      assert(result.toList() == List(1, 1, 2, 1, 2, 3))
    }

    it("a play around with streams") {


      val playone = Stream.ones.map(_ + 1).exists(_ % 2 == 0)

      val takeWhile = Stream.ones.takeWhile( _ == 1)

      val forAll = Stream.ones.forAll( _ == 1)

      assert(playone)


    }
  }
}
