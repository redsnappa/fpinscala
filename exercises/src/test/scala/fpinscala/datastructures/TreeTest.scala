package fpinscala.datastructures

class TreeTest extends org.scalatest.FunSuite {

  test("Tree size - should return the number of the branches and leaf"){
    val tree = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Leaf(10))
    assert(Tree.size(tree) == 7)
  }

  test("Tree maximum - should return the max value in the tree"){
    val tree = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Leaf(16))
    assert(Tree.maximum(tree) == 16)
  }

  test("Tree depth - should return the maximum path length from the root to any leaf"){
    val tree = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Branch(Leaf(7), Leaf(4)))
    assert(Tree.depth(tree) == 3)
  }

  test("Tree map - applies a function to every value in the tree"){
    val input = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Branch(Leaf(7), Leaf(4)))
    val op = (x:Int) => x * 2
    val expectd = Branch(Branch(Leaf(12), Branch(Leaf(18), Leaf(20))), Branch(Leaf(14), Leaf(8)))

    assert(Tree.map(input)(op) == expectd)
  }

  test("Tree fold - should reduce a T to a value given a function"){
    val input = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Branch(Leaf(7), Leaf(4)))
    val leafFun = (y:Int) => y
    val branchFun = (x:Int, y:Int) => x + y
    val expected:Int = 36
    val result:Int = Tree.fold(input)(leafFun)(branchFun)

    assert(result == expected)
  }

  test("Tree size2 - should return size 1 for single Leaf"){
    val tree = Leaf(6)
    assert(Tree.size2(tree) == 1)
  }

  test("Tree size2 - should return size 3 for single Branch with 2 Leaves"){
    val tree = Branch(Leaf(6), Leaf(7))
    assert(Tree.size2(tree) == 3)
  }

  test("Tree size2 - should return the number of the branches and leaf"){
    val tree = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Leaf(10))
    assert(Tree.size2(tree) == 7)
  }

  test("Tree maximum2 - should return the max value in the tree"){
    val tree = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Leaf(16))
    assert(Tree.maximum2(tree) == 16)
  }

  test("Tree depth2 - should return the maximum path length from the root to any leaf"){
    val tree = Branch(Branch(Leaf(6), Branch(Leaf(9), Leaf(10))), Branch(Leaf(7), Leaf(4)))
    assert(Tree.depth2(tree) == 3)
  }

}
