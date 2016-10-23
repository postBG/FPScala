package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight should correctly handle an array of size 1") {
    val output = new Array[Float](1)
    lineOfSight(Array[Float](0f), output)
    assert(output.toList == List(0f))
  }

  test("lineOfSight should correctly handle an array of size 2") {
    val output = new Array[Float](2)
    lineOfSight(Array[Float](0f, 5f), output)
    assert(output.toList == List(0f, 5f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweepSequential should correctly handle the chunk 1 until 2 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 2)
    assert(res == 1f)
  }

  test("upsweepSequential should correctly handle the chunk 1 until 2 of an array of 2 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f), 1, 2)
    assert(res == 1f)
  }

  test("upsweepSequential should correctly handle the chunk 0 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 0, 4)
    assert(res == 4f)
  }

  test("upsweep should correctly handle the chunk 0 until 4 of an array of 4 elements") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f)
    val res = upsweep(input, 0, input.length, 2)
    assert(res == Node(Leaf(0, 2, 1f), Leaf(2, 4, 4f)))
    assert(res.maxPrevious == 4f)
  }

  test("upsweep should correctly handle the chunk 0 until 4 of an array of 8 elements") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f, 10f, 11f, 12f, 13f, 14f)
    val res = upsweep(input, 0, 4, 2)
    assert(res == Node(Leaf(0, 2, 1f), Leaf(2, 4, 4f)))
    assert(res.maxPrevious == 4f)
  }

  test("upsweep should correctly handle the chunk 0 until 5 of an array of 5 elements") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f, 100f)
    val res = upsweep(input, 0, input.length, 2)
    assert(res == Node(Leaf(0, 2, 1f), Node(Leaf(2, 3, 4f), Leaf(3, 5, 25f))))
    assert(res.maxPrevious == 25f)
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is two") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 2f, 1, 4)
    assert(output.toList == List(0f, 2f, 4f, 4f))
  }

  test("downsweep should correctly handle a 4 element array when the starting angle is two") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)
    val t = Node(Leaf(0, 2, 1f), Leaf(2, 4, 4f))

    downsweep(input, output, 0f, t)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep should correctly handle the chunk 0 until 5 of an array of 5 elements with really big startingAngle") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f, 100f)
    val output = new Array[Float](5)
    val t = Node(Leaf(0, 2, 1f), Node(Leaf(2, 3, 4f), Leaf(3, 5, 25f)))

    downsweep(input, output, 100f, t)
    assert(output.toList == List(100f, 100f, 100f, 100f, 100f))
  }

  test("downsweep should correctly handle the chunk 0 until 5 of an array of 5 elements") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f, 100f)
    val output = new Array[Float](5)
    val t = Node(Leaf(0, 2, 1f), Node(Leaf(2, 3, 4f), Leaf(3, 5, 25f)))

    downsweep(input, output, 0f, t)
    assert(output.toList == List(0f, 1f, 4f, 4f, 25f))
  }

  test("parLineOfSight test1") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f)
    val output = new Array[Float](4)

    parLineOfSight(input, output, 2)
    assert(output.toList === List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight test2") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 9f, 100f)
    val output = new Array[Float](5)

    parLineOfSight(input, output, 2)
    assert(output.toList === List(0f, 1f, 4f, 4f, 25f))
  }

  test("parLineOfSight test3") {
    val input: Array[Float] = Array[Float](0f, 1f, 8f, 3f, 100f)
    val output = new Array[Float](5)

    parLineOfSight(input, output, 1)
    assert(output.toList === List(0f, 1f, 4f, 4f, 25f))
  }
}

