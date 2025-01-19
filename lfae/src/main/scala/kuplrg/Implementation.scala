package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def strict(v: Value): Value = v match
    case ExprV(e, env) => strict(interp(e, env))
    case _             => v

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => NumV(n)

    case Add(l, r) =>
      (strict(interp(l, env)), strict(interp(r, env))) match
        case (NumV(l), NumV(r)) => NumV(l + r)
        case _                  => error(s"invalid operation")

    case Mul(l, r) =>
      (strict(interp(l, env)), strict(interp(r, env))) match
        case (NumV(l), NumV(r)) => NumV(l * r)
        case _                  => error(s"invalid operation")

    case Id(n) => env.getOrElse(n, error(s"free identifier"))

    case Fun(p, b) => CloV(p, b, env)

    case App(f, arg) =>
      strict(interp(f, env)) match
        case CloV(p, b, fenv) => interp(b, fenv + (p -> ExprV(arg, env)))
        case _                => error(s"not a function")
}
