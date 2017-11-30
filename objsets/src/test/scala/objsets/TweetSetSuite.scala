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

  ignore("futzing with contains") {
    val t1 = new Tweet("u1", "aoeu", 11)
    val t2 = new Tweet("u2", "aeouaoeu", 11)
    val t3 = new Tweet("u3", "aoaeoueu", 11)

    val s1 = new Empty
    val s2 = s1.incl(t1)

    assert(s2.contains(t1))
    assert(!s2.contains(t2))
    assert(!s1.contains(t1))
  }

  ignore("build-a-bear") {
    val t5 = new Tweet("t5", "t5", 11)
    val t9 = new Tweet("t9", "t9", 11)
    val t8 = new Tweet("t8", "t8", 11)
    val t4 = new Tweet("t4", "t4", 11)

    val e = new Empty
    val s1 = e.incl(t5)
    val s2 = s1.incl(t4)
    val s3 = s2.incl(t9)
    val s4 = s3.incl(t8)

//    s4.foreach(t => println(t))
    assert(size(s4) === 4)

    val s5 = s4.incl(t4)
    assert(size(s5) === 4)
  }

  test("filterAcc") {
    new TestSets {
      val e = new Empty

      val always: Tweet => Boolean = t => true
      val never: Tweet => Boolean = t => false

      val f = e.filter(always)
      assert(size(f) === 0)

      val one = e.incl(c)
      val gone = one.filter(never)

      assert(size(gone) === 0)

      val twin = one.filter(always)

      assert(size(twin) === 1)
      assert(twin.contains(c))
    }
  }

  test("nontrivial predicate") {
    new TestSets {
      // set5 has a, b, c, and d.
      val under_10_retweets: Tweet => Boolean = t => t.retweets < 10

      val lotsa = set5.filter(under_10_retweets)

      assert(size(lotsa) == 2)
      assert(lotsa.contains(c))
      assert(lotsa.contains(d))
    }
  }

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

  ignore("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  ignore("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  ignore("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  ignore("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  }
