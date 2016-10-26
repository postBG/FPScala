package kmeans

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify' with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point,GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("'classify' with data parallelism should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkParClassify(points, means, expected)
  }

  test("'classify' with data parallelism should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkParClassify(points, means, expected)
  }

  test("'classify' with data parallelism should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkParClassify(points, means, expected)
  }


  def checkUpdate(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point], expected: GenSeq[Point]) {
    assert(update(classified, oldMeans).toString == expected.toString,
      s"update($classified, $oldMeans) should equal to $expected")
  }

  test("'update' should work for empty 'points' and empty 'means'") {
    val oldMeans: GenSeq[Point] = IndexedSeq()
    val classified = GenMap[Point, GenSeq[Point]]()
    val expected: GenSeq[Point] = IndexedSeq()
    checkUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val mean = new Point(1, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val classified = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    val expected: GenSeq[Point] = IndexedSeq(mean)
    checkUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 1)
    val p2 = new Point(1, -1, 1)
    val p3 = new Point(-1, 1, 1)
    val p4 = new Point(-1, -1, 1)

    val mean = new Point(0, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val classified = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    val expected: GenSeq[Point] = IndexedSeq(new Point(0, 0, 1))

    checkUpdate(classified, oldMeans, expected)
  }

  test("'update' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 1)
    val p2 = new Point(1, -1, 1)
    val p3 = new Point(-1, 1, 1)
    val p4 = new Point(-1, -1, 1)

    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val classified = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    val expected: GenSeq[Point] = IndexedSeq(new Point(1, 0, 1), new Point(-1, 0, 1))
    checkUpdate(classified, oldMeans, expected)
  }

  def checkParUpdate(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point], expected: GenSeq[Point]) {
    assert(update(classified.par, oldMeans.par).toList.toString == expected.toList.toString,
      s"update($classified, $oldMeans) should equal to $expected")
  }

  test("'parallel update' should work for empty 'points' and empty 'means'") {
    val oldMeans: GenSeq[Point] = IndexedSeq()
    val classified = GenMap[Point, GenSeq[Point]]()
    val expected: GenSeq[Point] = IndexedSeq()
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'parallel update' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val mean = new Point(1, 1, 1)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val classified = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    val expected: GenSeq[Point] = IndexedSeq(mean)
    checkParUpdate(classified, oldMeans, expected)
  }

  test("'parallel update' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 1)
    val p2 = new Point(1, -1, 1)
    val p3 = new Point(-1, 1, 1)
    val p4 = new Point(-1, -1, 1)

    val mean = new Point(0, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean)
    val classified = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    val expected: GenSeq[Point] = IndexedSeq(new Point(0, 0, 1))

    checkParUpdate(classified, oldMeans, expected)
  }

  test("'parallel update' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 1)
    val p2 = new Point(1, -1, 1)
    val p3 = new Point(-1, 1, 1)
    val p4 = new Point(-1, -1, 1)

    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val classified = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    val expected: GenSeq[Point] = IndexedSeq(new Point(1, 0, 1), new Point(-1, 0, 1))
    checkParUpdate(classified, oldMeans, expected)
  }

  test("converged test1") {
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val newMeans: GenSeq[Point] = IndexedSeq(new Point(1, 0, 1), new Point(-1, 0, 1))
    assert(converged(100)(oldMeans, newMeans))
  }

  test("converged test2: both far") {
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val newMeans: GenSeq[Point] = IndexedSeq(new Point(1, 0, 1), new Point(-1, 0, 1))
    assert(!converged(0.5)(oldMeans, newMeans))
  }

  test("converged test3: one pass, one fail") {
    val mean1 = new Point(1, 0, 4)
    val mean2 = new Point(-1, 0, 0)
    val oldMeans: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val newMeans: GenSeq[Point] = IndexedSeq(new Point(1, 0, 1), new Point(-1, 0, 1))
    assert(!converged(2)(oldMeans, newMeans))
  }

  test("converged test4: empty means") {
    val oldMeans: GenSeq[Point] = IndexedSeq()
    val newMeans: GenSeq[Point] = IndexedSeq()
    assert(converged(0)(oldMeans, newMeans))
  }
}


  
