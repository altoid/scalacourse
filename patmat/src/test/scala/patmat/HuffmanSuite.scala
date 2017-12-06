package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}

  test("weight of a leaf") {
    val l1 = Leaf('x', 42)
    assert(weight(l1) === 42)

    val c:CodeTree = l1
    assert(weight(c) === 42)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("make code tree") {
    val x = makeCodeTree(Leaf('a',2), Leaf('b',3))
    assert(x === Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5))
  }

  ignore("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  ignore("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  ignore("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("make ordered leaf list") {
    val chars = List('x', 'y', 'x', 'a', 'y', 'x', 'p', 'a')
    val freqs = times(chars)
    val x = makeOrderedLeafList(freqs)
    assert(x === List(Leaf('p',1), Leaf('y',2), Leaf('a',2), Leaf('x',3)))
  }

  test("until") {
    val freqs = times(string2Chars("xyxayxpa"))
    val leaves = makeOrderedLeafList(freqs)

    println(leaves)
    val combo = until(singleton, combine)(leaves)
    println(combo)
    assert(combo.size === 1)
  }

  test("combine 1") {
    val chars = string2Chars("scala_is_way_too_bitchy")

    // combine requires its input list to be sorted.
    val leaves = makeOrderedLeafList(times(chars)) sortWith (_.weight < _.weight)

    val combo = combine(leaves)

    assert(combo === List(Leaf('h',1), Leaf('w',1),
      Fork(Leaf('b',1),Leaf('l',1),List('b', 'l'),2),
      Leaf('s',2), Leaf('y',2), Leaf('t',2),
      Leaf('i',2), Leaf('c',2), Leaf('o',2),
      Leaf('a',3), Leaf('_',4)))
  }

  test("combine 2") {
    val chars = string2Chars("x")
    val leaves = makeOrderedLeafList(times(chars)) sortWith(_.weight < _.weight)
    assert(singleton(leaves) === true)
  }
}
