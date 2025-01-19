package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def numOp(op: (BigInt, BigInt) => BigInt)(l: Value, r: Value): Value = (l, r) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case _ => error(s"invalid operation")
  val numAdd: (Value, Value) => Value = numOp(_ + _)
  val numMul: (Value, Value) => Value = numOp(_ * _)

  def lookupId(name: String, env: Env): Value = env.getOrElse(name, error(s"free identifier"))
  

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = expr match
    case Num(n) => k(NumV(n))
    case Add(l, r) =>
      interpCPS(l, env, {
        lv => interpCPS(r, env, {
          rv => k(numAdd(lv, rv))
        })
      })
    
    case Mul(l, r) =>
      interpCPS(l, env, {
        lv => interpCPS(r, env, {
          rv => k(numMul(lv, rv))
        })
      })

    case Id(x) => k(lookupId(x, env))
    case Fun(p, b) => k(CloV(p, b, env))
    case App(f, a) => interpCPS(f, env, fv => fv match
      case CloV(p, b, fenv) =>
        interpCPS(a, env, av => {
          interpCPS(b, fenv + (p -> av), k)
        })
      case _ => error(s"not a function")
    )

  def reduce(k: Cont, s: Stack): (Cont, Stack) = (k, s) match
    case (EvalK(env, expr, k), s) => expr match
      case Num(n) => (k, NumV(n) :: s)
      case Add(l, r) => (EvalK(env, l, EvalK(env, r, AddK(k))), s)
      case Mul(l, r) => (EvalK(env, l, EvalK(env, r, MulK(k))), s)
      case Id(x) => (k, lookupId(x, env) :: s)
      case Fun(p, b) => (k, CloV(p, b, env) :: s)
      case App(f, a) => (EvalK(env, f, EvalK(env, a, AppK(k))), s)
    
    case (AddK(k), r :: l :: s) => (k, numAdd(r, l) :: s)
    case (MulK(k), r :: l :: s) => (k, numMul(r, l) :: s)
    case (AppK(k), a :: f :: s) => f match
      case CloV(p, b , fenv) => (EvalK(fenv + (p -> a), b, k), s)
      case _ => error (s"not a function")
}
