package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)       => n
    case Add(l, r)    => interp(l, env) + interp(r, env)
    case Mul(l, r)    => interp(l, env) * interp(r, env)
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x)        => env.getOrElse(x, error(s"free identifier: $x"))

  def freeIds(expr: Expr): Set[String] = expr match
    case Num(n)       => Set()
    case Add(l, r)    => freeIds(l) ++ freeIds(r)
    case Mul(l, r)    => freeIds(l) ++ freeIds(r)
    case Val(x, e, b) => freeIds(e) ++ (freeIds(b) - x)
    case Id(x)        => Set(x)

  def bindingIds(expr: Expr): Set[String] = expr match
    case Num(n)       => Set()
    case Add(l, r)    => bindingIds(l) ++ bindingIds(r)
    case Mul(l, r)    => bindingIds(l) ++ bindingIds(r)
    case Val(x, e, b) => bindingIds(e) ++ bindingIds(b) + x
    case Id(x)        => Set()

  def boundIds(expr: Expr): Set[String] = expr match
    case Num(n)    => Set()
    case Add(l, r) => boundIds(l) ++ boundIds(r)
    case Mul(l, r) => boundIds(l) ++ boundIds(r)
    case Val(x, e, b) =>
      if (freeIds(b).contains(x)) boundIds(e) ++ boundIds(b) + x
      else boundIds(e) ++ boundIds(b)
    case Id(x) => Set()

  def shadowedIds(expr: Expr): Set[String] = expr match
    case Num(n)    => Set()
    case Add(l, r) => shadowedIds(l) ++ shadowedIds(r)
    case Mul(l, r) => shadowedIds(l) ++ shadowedIds(r)
    case Val(x, e, b) =>
      if (bindingIds(b).contains(x)) shadowedIds(e) ++ shadowedIds(b) + x
      else shadowedIds(e) ++ shadowedIds(b)
    case Id(x) => Set()
}
