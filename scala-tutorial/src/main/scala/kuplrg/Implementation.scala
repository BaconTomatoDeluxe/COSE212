package kuplrg

object Implementation extends Template {

  def sqsum(x: Int, y: Int): Int = x * x + y * y

  def concat(left: String, right: String): String = left + right

  def subN(n: Int): Int => Int = _ - n

  def twice(f: Int => Int): Int => Int =
    (x: Int) => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    (x: Int) => f(g(x))

  def sumOnlyOdd(l: List[Int]): Int =
    l.filter(_ % 2 == 1)
      .foldLeft(0)(_ + _)

  def foldWith(f: (Int, Int) => Int): List[Int] => Int =
    (l: List[Int]) => l.foldLeft(0)(f)

  def toSet(l: List[Int], from: Int): Set[Int] =
    l.drop(from).toSet

  def getOrZero(map: Map[String, Int], key: String): Int =
    map.get(key).getOrElse(0)

  def setMinus(s1: Set[Int], s2: Set[Int]): Set[Int] =
    s1 -- s2

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def has(value: Int): Tree => Boolean =
    case Tree.Leaf(v) => v == value
    case Tree.Branch(left, v, right) =>
      v == value || has(value)(left) || has(value)(right)

  def maxDepthOf(value: Int): Tree => Option[Int] =
    case Tree.Leaf(v) if v == value => Some(0)
    case Tree.Branch(left, v, right) =>
      (maxDepthOf(value)(left), maxDepthOf(value)(right)) match
        case (Some(ld), Some(rd)) => Some(Math.max(ld, rd) + 1)
        case (Some(ld), None)     => Some(ld + 1)
        case (None, Some(rd))     => Some(rd + 1)
        case _                    => if (v == value) Some(0) else None
    case _ => None

  def mul(t: Tree): Int = t match
    case Leaf(value)                => value
    case Branch(left, value, right) => mul(left) * value * mul(right)

  def countLeaves(t: Tree): Int = t match
    case Leaf(value)                => 1
    case Branch(left, value, right) => countLeaves(left) + countLeaves(right)

  def postOrder(t: Tree): List[Int] = t match
    case Leaf(value) => List(value)
    case Branch(left, value, right) =>
      postOrder(left) ++ postOrder(right) ++ List(value)

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def countLiterals(expr: BE): Int = expr match
    case True  => 1
    case False => 1
    case And(left, right) =>
      countLiterals(left) + countLiterals(right)
    case Or(left, right) =>
      countLiterals(left) + countLiterals(right)
    case Not(innerexpr) => countLiterals(innerexpr)

  def countNots(expr: BE): Int = expr match
    case Not(innerexpr) => 1 + countNots(innerexpr)
    case And(left, right) =>
      countNots(left) + countNots(right)
    case Or(left, right) =>
      countNots(left) + countNots(right)
    case _ => 0

  def depth(expr: BE): Int = expr match
    case True  => 0
    case False => 0
    case And(left, right) =>
      Math.max(depth(left), depth(right)) + 1
    case Or(left, right) =>
      Math.max(depth(left), depth(right)) + 1
    case Not(innerexpr) => depth(innerexpr) + 1

  def getString(expr: BE): String = expr match
    case True  => "true"
    case False => "false"
    case And(left, right) =>
      "(" + getString(left) + " & " + getString(right) + ")"
    case Or(left, right) =>
      "(" + getString(left) + " | " + getString(right) + ")"
    case Not(innerexpr) => "!" + getString(innerexpr)

  def eval(expr: BE): Boolean = expr match
    case True             => true
    case False            => false
    case And(left, right) => eval(left) && eval(right)
    case Or(left, right)  => eval(left) || eval(right)
    case Not(innerexpr)   => !eval(innerexpr)
}
