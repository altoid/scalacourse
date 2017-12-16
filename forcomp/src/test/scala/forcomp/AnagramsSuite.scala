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

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract word - word") {
    // subtracting an occurrence list from itself should give the empty list.
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val result = subtract(lard, lard)
    assert(result === List())
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
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

  test("loadDictionary") {
    // test that loadDictionary gives us at least one word.
    val werdz = loadDictionary
    assert(werdz.size != 0)
    assert(werdz.head.size != 0)
  }

  test("no such word") {
    val findme = "zyzzva"  // not in the dictionary
    val occ = wordOccurrences(findme)
    intercept[NoSuchElementException] {
      val r = dictionaryByOccurrences(occ)
      println(r)
    }
  }

  ignore("futz") {
    /**
      * Given a list of positive ints <reference>,
      * @param reference a list of positive ints
      * @param bumpMe a list of nonnegative ints, such that bumpMe[i] <= reference[i]
      * @return a list of nonnegative ints the successor of bumpMe.  repeated calls
      *         to next() will give all lists of ints L where 0 <= L[i] <= reference[i].
      */
    def next(reference: List[Int], bumpMe: List[Int]): List[Int] = {
      def helper(reference: List[Int], incMe: List[Int]): List[Int] = {
        if (incMe.isEmpty) List()
        else {
          var n = incMe.head + 1
          if (n <= reference.head) n :: incMe.tail
          else 0 :: helper(reference.tail, incMe.tail)
        }
      }

      if (bumpMe.sameElements(reference)) bumpMe
      else {
        helper(reference, bumpMe)
      }
    }

    val test = List(('a', 1), ('b', 3), ('c', 2))
    val (letters, counts) = test.unzip
    var n = List.fill(counts.length)(0)
    println(letters)
    println(counts)

    var result = List[List[(Char, Int)]]()
    val zpd: List[(Char, Int)] = (letters zip n).filter(x => x._2 > 0)
    result = zpd :: result
    while (!n.sameElements(counts)) {
      n = next(counts, n)
      val zpd: List[(Char, Int)] = (letters zip n).filter(x => x._2 > 0)
      result = zpd :: result
    }

    println(result)
    println(result.length)
  }

  test("futz more") {
    /**
      * take each subset
      * obtain words for it
      * for each word, subtract from the sentence occurrences
      * do it again
      * keep going until the subtraction gives us an empty list --> win
      * or until we get a subset that doesn't correspond to any word --> lose
      */

    val s: Sentence = List("linux", "rulez")

    val sentence_occurrences = sentenceOccurrences(s)

    def drillDown(sentence_occurrences: Occurrences, level: Int, acc: List[List[Word]]): Unit = {
      if (sentence_occurrences.isEmpty) println(acc)
      val subsets = combinations(sentence_occurrences)
      for (subset <- subsets) {
        try {
          val d = dictionaryByOccurrences(subset)
//          println(" " * 4 * level + sentence_occurrences)
//          println(" " * 4 * level + d)

          val remainder = subtract(sentence_occurrences, subset)
          drillDown(remainder, level + 1, d :: acc)
        }
        catch {
          case what: NoSuchElementException => None // println(" " * 4 * level + subset + " didn't work")
        }
      }
    }

    drillDown(sentence_occurrences, 0, List[List[Word]]())
  }
}
