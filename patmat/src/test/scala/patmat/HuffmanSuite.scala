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

    val testTree: CodeTree = createCodeTree(string2Chars("this and that and whatever"))
    val letter_r = List[Bit](0, 1, 0, 1, 1)
    val letter_v = List[Bit](0, 1, 0, 1, 0)
    val letter_e = List[Bit](1, 1, 1, 1)
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

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
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

    val combo = until(singleton, combine)(leaves)
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

  test("decode singleton") {
    val tree = createCodeTree(string2Chars("r"))
    val letter_r = List[Bit](1)

    val d = decode(tree, letter_r)
    assert(d.size === 1)
    assert(d.mkString === "r")

    val d2 = decode(tree, letter_r ++ letter_r)
    assert(d2.size === 2)
    assert(d2.mkString === "rr")
  }

  test("decode") {
    new TestTrees {

      val d = decode(testTree, letter_r)
      assert(d.size === 1)
      assert(d.mkString === "r")

      val d2 = decode(testTree, letter_r ++ letter_v)
      assert(d2.size === 2)
      assert(d2.mkString === "rv")

      val d3 = decode(testTree, letter_r ++ letter_v ++ letter_e)
      assert(d3.size === 3)
      assert(d3.mkString === "rve")
    }
  }

  test("ooh la la") {
    assert(decodedSecret.mkString === "huffmanestcool") // vraiment
  }

  test("encode") {
    new TestTrees {
      val rve_bits = letter_r ++ letter_v ++ letter_e

      val test_bits = encode(testTree)(string2Chars("rve"))
      assert(test_bits === rve_bits)
    }
  }

  test("encode singleton") {
    val tree = createCodeTree(string2Chars("r"))

    val test_bits = encode(tree)(string2Chars("rr"))
    assert(test_bits === List[Bit](1, 1))
  }

  test("convert") {
    new TestTrees {
//      println(testTree)
      val table = convert(testTree)
//      println(table)

      assert(codeBits(table)('r') === letter_r)
    }
  }

  test("quickEncode") {
    new TestTrees {
      // encode a string, decode it, and see if we get back the original
      val text = "headwaiter"
      val hufbits = quickEncode(testTree)(string2Chars(text))

      assert(decode(testTree, hufbits).mkString === text)
    }
  }
}
