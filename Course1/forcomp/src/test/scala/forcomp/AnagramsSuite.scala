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

  test("wordOccurrences: RobertaA") {
    assert(wordOccurrences("RobertaA") === List(('a', 2), ('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: ee abcd E DDAA") {
    assert(sentenceOccurrences(List("ee", "abcd", "E", "DDAA")) === List(('a', 3), ('b', 1), ('c', 1), ('d', 3), ('e', 3)))
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

  test("test insertAll") {
    val occurrences = List(wordOccurrences("bbB"), wordOccurrences("ccddE"))
    assert(insertAll(occurrences)('a', 3).toSet === Set(List(('a', 3), ('b', 3)), List(('a', 3), ('c', 2), ('d', 2), ('e', 1))))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: aaa") {
    val aaa = List(('a', 3))
    val aaacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('a', 3))
    )
    assert(combinations(aaa).toSet === aaacomb.toSet)
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

  test("combinations: abc") {
    val abc = List(('a', 1), ('b', 1), ('c', 1))
    val abccomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('c', 1)),
      List(('a', 1), ('b', 1)),
      List(('b', 1), ('c', 1)),
      List(('a', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1))
    )

    assert(combinations(abc).toSet === abccomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: larrd - empty") {
    val larrd = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    val empty = List()
    val after = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    assert(subtract(larrd, empty) === after)
  }

  test("subtract: laaarrd - rada") {
    val laaarrd = List(('a', 3), ('d', 1), ('l', 1), ('r', 2))
    val rada = List(('a', 2), ('r', 1), ('d', 1))
    val lar = List(('a', 1), ('l', 1), ('r', 1))
    assert(subtract(laaarrd, rada) === lar)
  }

  test("mergeWith test1"){
    val sentences = List(List("go", "to", "bathroom"), List("are", "dead"))
    val expected = Set(List("we", "go", "to", "bathroom"), List("we", "are", "dead"), List("you", "go", "to", "bathroom"), List("you", "are", "dead"))
    assert(mergeWith(sentences)(List("we", "you")).toSet === expected)
  }

  test("mergeWith test2"){
    val sentences = List(List("go", "to", "bathroom"), List("are", "dead"))
    assert(mergeWith(sentences)(List()).toSet === Set())
  }

  test("anagram test1"){
    val occurrences = sentenceOccurrences(List("Yes", "man"))
    val expected = Set(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )
    assert(anagram(occurrences).toSet === expected)
  }

  test("anagram test2"){
    val occurrences = sentenceOccurrences(List("Linux", "rulez"))
    val expected = Set(
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
    assert(anagram(occurrences).toSet === expected)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
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

}
