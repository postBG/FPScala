package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union then filter") {
    new TestSets {
      assert(size(set1.union(set1).union(set2).union(set3).union(set5).filter(tweet => tweet.retweets > 8)) == 3)
    }
  }

  test("mostRetweeted on empty") {
    new TestSets {
      intercept[NoSuchElementException](set1.mostRetweeted)
      assert(true)
    }
  }

  test("mostRetweeted on set5") {
    new TestSets {
      assert(set5.mostRetweeted.retweets == 20)
    }
  }

  test("mostRetweeted on set4c") {
    new TestSets {
      assert(set4c.mostRetweeted.retweets == 20)
    }
  }

  test("mostRetweeted on oneSet") {
    new TestSets {
      val newSet = set1.incl(new Tweet("x", "x body", 98))
      assert(newSet.mostRetweeted.retweets == 98)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: empty") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }

  test("decending: extreme most retweeted") {
    new TestSets {
      val newSet = set5.incl(new Tweet("x", "x body", 100))
      val trends = newSet.descendingByRetweet

      assert(!trends.isEmpty)
      assert(trends.head.user == "x")
      assert(trends.head.retweets == 100)

      assert(trends.tail.head.retweets == 20)
    }
  }

  test("isTweetOf test1") {
    new TestSets {
      val newSet = set5.incl(new Tweet("x", "x any", 100))
      val words = List("body")

      assert(size(newSet.filter(GoogleVsApple.isTweetOf(words))) == 4)
    }
  }

  test("isTweetOf test2"){
    new TestSets {
      val newSet = set5.incl(new Tweet("x", "x any", 100))
      val words = List("x", "a", "c")

      assert(size(newSet.filter(GoogleVsApple.isTweetOf(words))) == 3)
    }
  }

}
