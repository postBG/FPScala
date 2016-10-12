
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var redSum = 0
    var greenSum = 0
    var blueSum = 0
    var alphaSum = 0

    var count = 0

    var xOffset = -radius
    while(xOffset < radius + 1) {
      var yOffset = -radius

      while(yOffset < radius + 1) {
        if(isInside(x + xOffset, y + yOffset, src)){
          val pixel = src(x + xOffset, y + yOffset)

          redSum = redSum + red(pixel)
          greenSum = greenSum + green(pixel)
          blueSum = blueSum + blue(pixel)
          alphaSum = alphaSum + alpha(pixel)

          count = count + 1
        }

        yOffset = yOffset + 1
      }

      xOffset = xOffset + 1
    }

    rgba(redSum / count, greenSum / count, blueSum / count, alphaSum / count)
  }

  private def isInside(x: Int, y: Int, src: Img): Boolean = {
    isInsideBetween(x, 0, src.width) && isInsideBetween(y, 0, src.height)
  }

  private def isInsideBetween(num: Int, inclusiveMin: Int, exclusiveMax: Int):Boolean = {
    inclusiveMin <= num && num < exclusiveMax
  }

  def getIndexPairs(totalSize: Int, numTasks: Int): List[(Int, Int)] = {
    val stepSize = if (totalSize >= numTasks) totalSize / numTasks else 1
    val remains = if (totalSize >= numTasks) totalSize % numTasks else 0

    val starts = 0 until (totalSize - remains) by stepSize
    val ends = starts.tail :+ totalSize

    (starts zip ends).toList
  }
}
