package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(n)  => NumT
    case Bool(b) => BoolT
    case Add(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => NumT
        case _            => error(s"invalid operation")
    case Mul(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => NumT
        case _            => error(s"invalid operation")
    case Div(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => NumT
        case _            => error(s"invalid operation")
    case Mod(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => NumT
        case _            => error(s"invalid operation")
    case Eq(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => BoolT
        case _            => error(s"invalid operation")
    case Lt(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => BoolT
        case _            => error(s"invalid operation")
    case Val(name, init, body) =>
      typeCheck(body, tenv + (name -> typeCheck(init, tenv)))
    case Id(x)             => tenv.getOrElse(x, error(s"free identifier"))
    case Fun(p, pty, body) => ArrowT(pty, typeCheck(body, tenv + (p -> pty)))
    case Rec(x, p, pty, rty, body, scope) =>
      val tauu = typeCheck(body, tenv + (x -> ArrowT(pty, rty), (p -> pty)))
      typeCheck(scope, tenv + (x -> ArrowT(pty, rty)))
    case App(fun, arg) =>
      typeCheck(fun, tenv) match
        case ArrowT(paramTy, retTy) =>
          if (paramTy == typeCheck(arg, tenv)) retTy
          else error(s"invalid type")
        case _ => error(s"invalid type")
    case If(c, t, e) =>
      typeCheck(c, tenv) match
        case BoolT =>
          val tau = typeCheck(t, tenv)
          if (tau == typeCheck(e, tenv)) tau
          else error(s"invalid type")
        case _ => error(s"not a condition")

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)  => NumV(n)
    case Bool(b) => BoolV(b)
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
    case Val(name, init, body) =>
      interp(body, env + (name -> interp(init, env)))
    case Id(x)             => env.getOrElse(x, error(s"free identifier"))
    case Fun(p, pty, body) => CloV(p, body, () => env)
    case Rec(x, p, pty, rty, body, scope) =>
      lazy val envv: () => Env = () => (env + (x -> CloV(p, body, envv)))
      interp(scope, envv())
    case App(fun, arg) =>
      interp(fun, env) match
        case CloV(p, body, fenv) =>
          interp(body, fenv() + (p -> interp(arg, env)))
        case _ => error(s"not a function")
    case If(c, t, e) =>
      interp(c, env) match
        case BoolV(true)  => interp(t, env)
        case BoolV(false) => interp(e, env)
        case _            => error(s"not a condition")
}
