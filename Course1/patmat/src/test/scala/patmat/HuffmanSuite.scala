package patmat

import javax.swing.plaf.synth.Region

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t0 = Leaf('x', 4)
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of single leaf") {
    new TestTrees {
      assert(weight(t0) === 4)
    }
  }

  test("weight of t2") {
    new TestTrees {
      assert(weight(t2) == 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars of a single node") {
    new TestTrees {
      assert(chars(t0) === List('x'))
    }
  }

  test("chars of t1") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("test time1") {
    assert(times(string2Chars("aaaaabbbbcccdd")) === List(('b', 4), ('d', 2), ('a', 5), ('c', 3)))
  }

  test("test time2") {
    assert(times(string2Chars("x")) === List(('x', 1)))
  }

  test("times empty") {
    assert(times(List()) === List())
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for some frequency table2") {
    assert(makeOrderedLeafList(times(string2Chars("aaaaabbbbcccdd"))) === List(Leaf('d', 2), Leaf('c', 3), Leaf('b', 4), Leaf('a', 5)))
  }

  test("makeOrderedLeafList for some frequency table3") {
    assert(makeOrderedLeafList(times(string2Chars("x"))) === List(Leaf('x', 1)))
  }

  test("makeOrderedLeafList for some frequency empty") {
    assert(makeOrderedLeafList(List()) === List())
  }

  test("check singleton when really singleton") {
    assert(singleton(List(Leaf('c', 1))))
    assert(singleton(List(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3))))
  }

  test("check singleton when empty") {
    assert(!singleton(List()))
  }

  test("check singleton when more than two") {
    assert(!singleton(List(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3), Leaf('c', 1))))
  }

  test("test insert ordered") {
    val trees = List(Leaf('a', 3), Leaf('b', 6), Fork(Leaf('a', 6), Leaf('c', 7), List('a', 'c'), 13))
    val tree = Fork(Leaf('k', 1), Leaf('a', 3), List('k', 'a'), 4)
    assert(insertOrdered(tree, trees) == List(Leaf('a', 3), Fork(Leaf('k', 1), Leaf('a', 3), List('k', 'a'), 4), Leaf('b', 6), Fork(Leaf('a', 6), Leaf('c', 7), List('a', 'c'), 13)))
  }

  test("test insert ordered first") {
    val trees = List(Leaf('a', 3), Leaf('b', 6), Fork(Leaf('a', 6), Leaf('c', 7), List('a', 'c'), 13))
    val tree = Leaf('x', 1)
    assert(insertOrdered(tree, trees) == List(Leaf('x', 1), Leaf('a', 3), Leaf('b', 6), Fork(Leaf('a', 6), Leaf('c', 7), List('a', 'c'), 13)))
  }

  test("test insert ordered last") {
    val trees = List(Leaf('a', 3), Leaf('b', 6), Fork(Leaf('a', 6), Leaf('c', 7), List('a', 'c'), 13))
    val tree = Leaf('x', 100)
    assert(insertOrdered(tree, trees) == List(Leaf('a', 3), Leaf('b', 6), Fork(Leaf('a', 6), Leaf('c', 7), List('a', 'c'), 13), Leaf('x', 100)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of some leaf list2") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('c', 3), Leaf('x', 4), Leaf('e', 5))
    val combine1: List[CodeTree] = combine(leaflist)

    assert(combine1 === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('c', 3), Leaf('x', 4), Leaf('e', 5)))
    assert(combine(combine1) === List(Leaf('x', 4), Leaf('e', 5), Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('c', 3), List('e', 't', 'c'), 6)))
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('c', 3), Leaf('x', 4), Leaf('y', 5))

    val left: CodeTree = Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('c', 3), List('e', 't', 'c'), 6)
    val right: CodeTree = Fork(Leaf('x', 4), Leaf('y', 5), List('x', 'y'), 9)
    assert(until(singleton, combine)(leaflist) === List(Fork(left, right, List('e', 't', 'c', 'x', 'y'), 15)))
  }

  test("createCodeTree") {
    val chars = string2Chars("yyyyyxxxxccctte")

    val left: CodeTree = Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('c', 3), List('e', 't', 'c'), 6)
    val right: CodeTree = Fork(Leaf('x', 4), Leaf('y', 5), List('x', 'y'), 9)
    assert(createCodeTree(chars) === Fork(left, right, List('e', 't', 'c', 'x', 'y'), 15))
  }

  test("decode1") {
    assert(decodedSecret == string2Chars("huffmanestcool"))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abddaabbdd".toList)) === "abddaabbdd".toList)
    }
  }

  test("decode and encode a middle text should be identity") {
    val chars = string2Chars("yyyyyxxxxccctte")
    val tree = createCodeTree(chars)
    assert(decode(tree, encode(tree)("xyyxxcttectexxxxeeeccctct".toList)) === "xyyxxcttectexxxxeeeccctct".toList)
  }

  test("codeBits test1") {
    val table = List(('c', List(1, 0)), ('b', List(0)))
    assert(codeBits(table)('b') === List(0))
  }

  test("codeBits test2") {
    val table = List(('c', List(1, 0)), ('b', List(0)), ('d', List(1, 1)))
    assert(codeBits(table)('c') === List(1, 0))
  }

  test("mergeCodeTable test1") {
    val leftTable = List(('b', List(0)), ('c', List(1)))
    val rightTable = List(('d', List(0)), ('e', List(1)))
    val tables: CodeTable = mergeCodeTables(leftTable, rightTable)

    assert(tables.contains(('b', List(0, 0))))
    assert(tables.contains(('c', List(0, 1))))
    assert(tables.contains(('d', List(1, 0))))
    assert(tables.contains(('e', List(1, 1))))
  }

  test("mergeCodeTable test2") {
    val leftTable = List(('b', List(0)), ('c', List(1)))
    val rightTable = List(('d', List(0)), ('e', List(1)))
    val mergedSubTable = mergeCodeTables(leftTable, rightTable)

    val leaf = List(('a', List()))
    val tables: CodeTable = mergeCodeTables(leaf, mergedSubTable)

    assert(tables.contains(('a', List(0))))
    assert(tables.contains(('b', List(1, 0, 0))))
    assert(tables.contains(('c', List(1, 0, 1))))
    assert(tables.contains(('d', List(1, 1, 0))))
    assert(tables.contains(('e', List(1, 1, 1))))
  }

  test("decode and quickencode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quickencode a short text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abddaabbdd".toList)) === "abddaabbdd".toList)
    }
  }

  test("decode and quickencode a middle text should be identity") {
    val chars = string2Chars("yyyyyxxxxccctte")
    val tree = createCodeTree(chars)
    assert(decode(tree, quickEncode(tree)("xyyxxcttectexxxxeeeccctct".toList)) === "xyyxxcttectexxxxeeeccctct".toList)
  }
}
