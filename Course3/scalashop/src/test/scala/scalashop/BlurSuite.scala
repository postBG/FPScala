package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1 at 0,0"){
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 0, 0, 1) === 2)
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1 at 2,3"){
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 2, 3, 1) === 10)
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 2"){
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 2) === 9)

  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)
    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)
    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)
  }

  test("1x1 image blur") {
    val w = 1
    val h = 1
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 1

    VerticalBoxBlur.blur(src, dst, 0, 1, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 1)
  }

  test("makeIndexPair1") {
    val width = 12
    val numTasks = 3

    assert(getIndexPairs(width, numTasks).size === numTasks)
  }

  test("makeIndexPair2") {
    val width = 13
    val numTasks = 3

    assert(getIndexPairs(width, numTasks).size === numTasks)
  }

  test("makeIndexPair3") {
    val width = 1100
    val numTasks = 100

    assert(getIndexPairs(width, numTasks).size === numTasks)
  }

  test("makeIndexPair4") {
    val width = 1140
    val numTasks = 100

    assert(getIndexPairs(width, numTasks).size === numTasks)
  }

  test("makeIndexPair5") {
    val width = 105
    val numTasks = 1

    assert(getIndexPairs(width, numTasks).size === numTasks)
  }

  test("makeIndexPair6") {
    val width = 105
    val numTasks = 120

    assert(getIndexPairs(width, numTasks).size === 105)
  }

  test("VerticalBoxBlur.parBlur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    VerticalBoxBlur.parBlur(src, dst, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)
    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)
    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image for all") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 3, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 should correctly blur the entire 3x3 image for all with 1 task") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 1, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 should correctly blur the entire 3x3 image for all with 2 task") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 2, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 should correctly blur the entire 3x3 image for all with 8 task") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.parBlur(src, dst, 8, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 5)
    check(1, 2, 5)
    check(2, 2, 6)
  }
}