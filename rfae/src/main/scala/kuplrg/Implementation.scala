package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)   => NumV(n)
    case Bool(b)  => BoolV(b)
    case Id(name) => env.getOrElse(name, error(s"free identifier"))
    case Add(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => NumV(l + r)
        case _                  => error(s"invalid operation")
    case Mul(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => NumV(l * r)
        case _                  => error(s"invalid operation")
    case Div(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) =>
          if (r != 0) NumV(l / r) else error(s"invalid operation")
        case _ => error(s"invalid operation")
    case Mod(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) =>
          if (r != 0) NumV(l % r) else error(s"invalid operation")
        case _ => error(s"invalid operation")
    case Eq(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => if (l == r) BoolV(true) else BoolV(false)
        case _                  => error(s"invalid operation")
    case Lt(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => if (l < r) BoolV(true) else BoolV(false)
        case _                  => error(s"invalid operation")
    case Fun(p, b) => CloV(p, b, () => env)
    case Rec(n, p, b, s) =>
      lazy val newEnv: Env = env + (n -> CloV(p, b, () => newEnv))
      interp(s, newEnv)
    case App(f, arg) =>
      interp(f, env) match
        case CloV(p, b, fenv) => interp(b, fenv() + (p -> interp(arg, env)))
        case _                => error(s"not a function")
    case If(c, t, e) =>
      interp(c, env) match
        case BoolV(b) =>
          b match
            case true  => interp(t, env)
            case false => interp(e, env)
        case _ => error(s"not a boolean")
}
