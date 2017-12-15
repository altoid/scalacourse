package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences:  longer") {
    val sentence = "Next we implement another version of the method for entire sentences"
    assert(sentenceOccurrences(sentence.split(" ").toList) === List(('a',1), ('c',1), ('d',1), ('e',13), ('f',2), ('h',3), ('i',3), ('l',1), ('m',3), ('n',7), ('o',5), ('p',1), ('r',4), ('s',3), ('t',7), ('v',1), ('w',1), ('x',1)))
  }

  ignore("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  ignore("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  ignore("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  ignore("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }


  ignore("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  ignore("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  ignore("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  ignore("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  ignore("loadDictionary") {
    // test that loadDictionary gives us at least one word.
    val werdz = loadDictionary
    assert(werdz.size != 0)
    assert(werdz.head.size != 0)
  }

  test("futz") {
    //    println(wordAnagrams("spin"))
    // val reference = List(1, 3, 2).toArray


    //    println("test = " + test.mkString(","))
    //    println("result = " + result.mkString(","))
    //    assert(result.sameElements(test))
    //    result(0) = 42
    //    println("test = " + test.mkString(","))
    //    println("result = " + result.mkString(","))
    //    assert(!result.sameElements(test))

    def next(reference: Array[Int], bumpMe: Array[Int]): Array[Int] = {
      def helper(reference: Array[Int], incMe: Array[Int], i: Int): Array[Int] = {
        incMe(i) += 1
        if (incMe(i) <= reference(i)) incMe
        else {
          incMe(i) = 0
          helper(reference, incMe, i + 1)
        }
      }

      if (bumpMe.sameElements(reference)) bumpMe
      else {
        helper(reference, bumpMe, 0)
      }
    }

//    val test: Array[Int] = Array.fill[Int](reference.length)(0)
//    var n = test.clone()
//    println(n.mkString(" "))
//
//    n = next(reference, n)
//    println(n.mkString(" "))
//
//    n = next(reference, n)
//    println(n.mkString(" "))
//
//    n = next(reference, n)
//    println(n.mkString(" "))
//
//    while (!n.sameElements(reference)) {
//      n = next(reference, n)
//      println(n.mkString(" "))
//    }

    val test = List(('a', 1), ('b', 3), ('c', 2))
    val (letters, counts) = test.unzip
    val countsArr = counts.toArray
    var n = Array.fill[Int](counts.length)(0)
    println(letters)
    println(counts)

    val zpd: List[(Char, Int)] = (letters zip n.toList) // .filter(x => x._2 > 0)
    println(zpd)

    while (!n.sameElements(counts)) {
      n = next(countsArr, n)
      val zpd: List[(Char, Int)] = (letters zip n.toList) // .filter(x => x._2 > 0)
      println(zpd)
    }

  }
}
