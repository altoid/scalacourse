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

  trait BunchaTweets {
    val t1 = new Tweet("t1", "t1", 1)
    val t2 = new Tweet("t2", "t2", 2)
    val t3 = new Tweet("t3", "t3", 3)
    val t4 = new Tweet("t4", "t4", 4)
    val t5 = new Tweet("t5", "t5", 5)
    val t6 = new Tweet("t6", "t6", 6)

    val e = new Empty

    val s1 = e.incl(t1)
    val s12 = s1.incl(t2)
    val s123 = s12.incl(t3)

    val s4 = e.incl(t4)
    val s45 = s4.incl(t5)
    val s456 = s45.incl(t6)

    val allTweets = e.incl(t3).incl(t4).incl(t2).incl(t6).incl(t5).incl(t1)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  ignore("reverse") {
    new BunchaTweets {
      val tl = new Cons(t1, new Cons(t2, new Cons(t3, Nil)))
      val r = tl.reverse
      assert(r.head === t3)
      assert(r.tail.head === t2)
      assert(r.tail.tail.head === t1)
      assert(r.tail.tail.tail.isEmpty)
    }
  }

  ignore("remove manually") {
    new BunchaTweets {
      val mrt = allTweets.mostRetweeted
      val lighter = allTweets.remove(mrt)
      assert(size(lighter) == size(allTweets) - 1)

      val list = new Cons(mrt, Nil)

      val another = lighter.mostRetweeted
      val evenLighter = lighter.remove(another)

      val longer = new Cons(another, list)
    }
  }

  ignore("most retweeted - empty set") {
    val e = new Empty

    intercept[NoSuchElementException] {
      val t = e.mostRetweeted
    }
  }

  ignore("most retweeted - nonempty set") {
    new BunchaTweets {

      assert(size(allTweets) === 6)

      assert(allTweets.mostRetweeted == t6)
      assert(s12.mostRetweeted == t2)
      assert(s45.mostRetweeted == t5)
    }
  }

  ignore("most retweeted - singleton") {
    new BunchaTweets {
      val mrt = s1.mostRetweeted
      assert(mrt == t1)
    }
  }

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

  ignore("s1_u_s1") {
    new BunchaTweets {
      val s1_u_s1 = s1.union(s1)
      assert(size(s1_u_s1) === 1)
      assert(s1_u_s1.contains(t1))
    }
  }

  ignore("s1_u_s4") {
    new BunchaTweets {
      val s1_u_s4 = s1.union(s4)
      assert(size(s1_u_s4) === 2)
      assert(s1_u_s4.contains(t1))
      assert(s1_u_s4.contains(t4))
    }
  }

  ignore("s123_u_s456") {
    new BunchaTweets {
      val s123_u_s456 = s123.union(s456)
      assert(size(s123_u_s456) === 6)
      assert(s123_u_s456.contains(t1))
      assert(s123_u_s456.contains(t2))
      assert(s123_u_s456.contains(t3))
      assert(s123_u_s456.contains(t4))
      assert(s123_u_s456.contains(t5))
      assert(s123_u_s456.contains(t6))
    }
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

  ignore("filterAcc") {
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

  ignore("filter, nontrivial predicate") {
    new TestSets {
      // set5 has a, b, c, and d.
      val under_10_retweets: Tweet => Boolean = t => t.retweets < 10

      val lotsa = set5.filter(under_10_retweets)

      assert(size(lotsa) == 2)
      assert(lotsa.contains(c))
      assert(lotsa.contains(d))
    }
  }

  ignore("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  ignore("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  ignore("filter: 20 on set5") {
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

  test("tokenize") {
    val t = new Tweet("CNET",
      "Four most-useful new settings in iOS 6 http://t.co/LHFOCLnA",
      63)

    // need to pick off punctuation
    val tokens = t.text.split("""[ \p{Punct}]+""")
    assert(tokens.contains("settings"))
    assert(!tokens.contains("theomorphism"))

    val test_tokens = List("new", "settings", "potato")

    val not_found = List("meh", "guava", "meep")

    assert(not_found intersect tokens isEmpty)
    assert(test_tokens intersect tokens nonEmpty)
  }

  test("build apple set") {
    val apple_tokens = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
    val t1 = new Tweet("CNET",
      "Four most-useful new settings in iOS 6 http://t.co/LHFOCLnA",
      63)
    val t2 = new Tweet("CNET",
      "RT @CNETNews: Apple \"fell short\" with iOS 6 maps, and we are \"extremely sorry,\" CEO Tim Cook says in open letter http://t.co/t1U4497r",
      139)
    val t3 = new Tweet("mashable",
      "Book About Kickstarter Fails to Raise Enough Money on Kickstarter http://t.co/SpKYdnOx",
      59)

    // want a function that takes a list of tokens and returns a Tweet => Boolean predicate
    def filter_on_tokens(vendor_tokens: List[String]): Tweet => Boolean = {
      def anyfilter(tweet: Tweet): Boolean = {
//        val tokens = tweet.text.split("""[ \p{Punct}]+""")
//        vendor_tokens intersect tokens nonEmpty
        vendor_tokens.exists(tweet.text.contains(_))
      }
      anyfilter
    }
    val tw_set = (new Empty).incl(t1).incl(t2).incl(t3)
    val appleTweets = tw_set.filter(filter_on_tokens(apple_tokens))

    assert(size(appleTweets) === 2)
    assert(appleTweets.contains(t1))
    assert(appleTweets.contains(t2))
    assert(!appleTweets.contains(t3))
  }
}
