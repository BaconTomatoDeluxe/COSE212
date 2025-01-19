package kuplrg

import Implementation.*

class Spec extends SpecBase {
  // tests for `sqsum`
  test(sqsum(0, 0), 0)
  test(sqsum(2, 3), 13)
  test(sqsum(-3, 4), 25)
  test(sqsum(5, -8), 89)
  test(sqsum(-9, -10), 181)

  // tests for `concat`
  test(concat("Hello ", "World!"), "Hello World!")
  test(concat("COSE", "212"), "COSE212")
  test(concat("COSE", "215"), "COSE215")
  test(concat("Hello", " World!"), "Hello World!")
  test(concat("", "Hello"), "Hello")

  // tests for `subN`
  test(subN(3)(5), 2)
  test(subN(4)(13), 9)
  test(subN(243)(-942), -1185)
  test(subN(0)(10), 10)
  test(subN(-26)(15), 41)

  // tests for `twice`
  test(twice(_ + 3)(1), 7)
  test(twice(subN(3))(10), 4)
  test(twice(_ * 10)(42), 4200)
  test(twice(_ / 5)(124), 4)
  test(twice(2 - _)(5), 5)

  // tests for `compose`
  test(compose(_ + 3, _ * 2)(1), 5)
  test(compose(_ * 10, _ + 1)(42), 430)
  test(compose(subN(3), subN(2))(10), 5)
  test(compose(twice(subN(2)), twice(subN(1)))(10), 4)
  test(compose(9 - _, 11 - _)(5), 3)

  // tests for `sumOnlyOdd`
  test(sumOnlyOdd(List(2)), 0)
  test(sumOnlyOdd(List(1, 2, 3)), 4)
  test(sumOnlyOdd(List(4, 2, 3, 7, 5)), 15)
  test(sumOnlyOdd(Nil), 0)
  test(sumOnlyOdd(List(2, 1, 3, 5, 6)), 9)

  // tests for `foldWith`
  test(foldWith(_ + _)(List(1, 2, 3)), 6)
  test(foldWith(_ - _)(List(5, 9, 2, 3)), -19)
  test(foldWith(_ * 2 + _)(List(4, 7, 3, 2)), 68)
  test(foldWith((x, y) => (x + 10) / y)(List(5, 3, 4, 2, 3)), 5)
  test(foldWith((x, y) => (x ^ y) + 1)(List(1, 2, 3, 4, 5)), 14)

  // tests for `toSet`
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 0), Set(1, 2, 4, 5, 7))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 2), Set(2, 4, 7))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 4), Set(2, 4))
  test(toSet(List(1, 5, 2, 7, 4, 2, 4), 7), Set())
  test(toSet(List(1, 1, 1, 1, 2, 2, 2, 2), 4), Set(2))

  // tests for `getOrZero`
  val m: Map[String, Int] = Map("Park" -> 3, "Kim" -> 5)
  test(getOrZero(m, "Park"), 3)
  test(getOrZero(m, "Lee"), 0)
  test(getOrZero(m, "Kim"), 5)
  test(getOrZero(m, "park"), 0)
  test(getOrZero(m, ""), 0)

  // tests for `setMinus`
  test(setMinus(Set(1, 2, 3), Set(2, 3, 4)), Set(1))
  test(setMinus(Set(1, 2, 3), Set(4, 5, 6)), Set(1, 2, 3))
  test(setMinus(Set(1, 2, 3), Set(1, 2, 3, 4)), Set())
  test(setMinus(Set(1, 2, 3, 4), Set()), Set(1, 2, 3, 4))
  test(setMinus(Set(), Set(1, 2, 3, 4)), Set())

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  //  8
  val tree1: Tree = Leaf(8)

  //    4
  //   / \
  //  5   2
  //     / \
  //    8   3
  val tree2: Tree = Branch(Leaf(5), 4, Branch(Leaf(8), 2, Leaf(3)))

  //    7
  //   / \
  //  2   3
  //     / \
  //    5   1
  //   / \
  //  1   8
  val tree3: Tree = Branch(Leaf(2), 7, Branch(Branch(Leaf(1), 5, Leaf(8)), 3, Leaf(1)))

  //    1
  //   / \
  //  2   1
  //     / \
  //    2   1
  //       / \
  //      2   3
  val tree4: Tree = Branch(Leaf(2), 1, Branch(Leaf(2), 1, Branch(Leaf(2), 1, Leaf(3))))

  //         1
  //        / \
  //      4     2
  //     / \   / \
  //    1   3 1   5
  //   / \       / \
  //  1   2     2   3
  //               / \
  //              1   1
  val tree5: Tree = Branch(Branch(Branch(Leaf(1), 1, Leaf(2)), 4, Leaf(3)), 1, Branch(Leaf(1), 2, Branch(Leaf(2), 5, Branch(Leaf(1), 3, Leaf(1)))))

  // tests for `has`
  test(has(8)(tree1), true)
  test(has(7)(tree2), false)
  test(has(1)(tree3), true)
  test(has(1)(tree4), true)
  test(has(5)(tree5), true)

  // tests for `maxDepthOf`
  test(maxDepthOf(8)(tree1), Some(0))
  test(maxDepthOf(7)(tree2), None)
  test(maxDepthOf(1)(tree3), Some(3))
  test(maxDepthOf(1)(tree4), Some(2))
  test(maxDepthOf(3)(tree5), Some(3))

  // tests for `mul`
  test(mul(tree1), 8)
  test(mul(tree2), 960)
  test(mul(tree3), 1680)
  test(mul(tree4), 24)
  test(mul(tree5), 1440)

  // tests for `countLeaves`
  test(countLeaves(tree1), 1)
  test(countLeaves(tree2), 3)
  test(countLeaves(tree3), 4)
  test(countLeaves(tree4), 4)
  test(countLeaves(tree5), 7)

  // tests for `postOrder`
  test(postOrder(tree1), List(8))
  test(postOrder(tree2), List(5, 8, 3, 2, 4))
  test(postOrder(tree3), List(2, 1, 8, 5, 1, 3, 7))
  test(postOrder(tree4), List(2, 2, 2, 3, 1, 1, 1))
  test(postOrder(tree5), List(1, 2, 1, 3, 4, 1, 2, 1, 1, 3, 5, 2, 1))

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  // (true | false)
  val be1: BE = Or(True, False)

  // (!(true | false) & !(false | true))
  val be2: BE = And(Not(Or(True, False)), Not(Or(False, True)))

  // (!((false | !true) & false) & (true & !false))
  val be3: BE = And(Not(And(Or(False, Not(True)), False)), And(True, Not(False)))

  // !((!true | !false) & !(false | (!true & true)))
  val be4: BE = Not(And(Or(Not(True), Not(False)), Not(Or(False, And(Not(True), True)))))

  // !!!(!!!true & !!!false)
  val be5: BE = Not(Not(Not(And(Not(Not(Not(True))), Not(Not(Not(False)))))))

  // tests for `countLiterals`
  test(countLiterals(be1), 2)
  test(countLiterals(be2), 4)
  test(countLiterals(be3), 5)
  test(countLiterals(be4), 5)
  test(countLiterals(be5), 2)

  // tests for `countNots`
  test(countNots(be1), 0)
  test(countNots(be2), 2)
  test(countNots(be3), 3)
  test(countNots(be4), 5)
  test(countNots(be5), 9)

  // tests for `depth`
  test(depth(be1), 1)
  test(depth(be2), 3)
  test(depth(be3), 5)
  test(depth(be4), 6)
  test(depth(be5), 7)

  // tests for `eval`
  test(eval(be1), true)
  test(eval(be2), false)
  test(eval(be3), true)
  test(eval(be4), false)
  test(eval(be5), true)

  // tests for `getString`
  test(getString(be1), "(true | false)")
  test(getString(be2), "(!(true | false) & !(false | true))")
  test(getString(be3), "(!((false | !true) & false) & (true & !false))")
  test(getString(be4), "!((!true | !false) & !(false | (!true & true)))")
  test(getString(be5), "!!!(!!!true & !!!false)")

  /* Write your own tests */
}
