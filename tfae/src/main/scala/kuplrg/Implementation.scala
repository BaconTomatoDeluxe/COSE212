package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(n) => NumT
    case Add(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => NumT
        case _            => error(s"unexpected type")
    case Mul(l, r) =>
      (typeCheck(l, tenv), typeCheck(r, tenv)) match
        case (NumT, NumT) => NumT
        case _            => error(s"unexpected type")
    case Val(name, init, body) =>
      typeCheck(body, tenv + (name -> typeCheck(init, tenv)))
    case Id(x) => tenv.getOrElse(x, error(s"free identifier"))
    case Fun(param, ty, body) =>
      ArrowT(ty, typeCheck(body, tenv + (param -> ty)))
    case App(fun, arg) =>
      typeCheck(fun, tenv) match
        case ArrowT(paramTy, retTy) =>
          val t1 = typeCheck(arg, tenv)
          if (paramTy == t1) retTy
          else error(s"unexpected type")
        case _ => error(s"invalid operation")

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => NumV(n)
    case Add(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => NumV(l + r)
        case _                  => error(s"invalid operation")
    case Mul(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => NumV(l * r)
        case _                  => error(s"invalid operation")
    case Val(name, init, body) =>
      interp(body, env + (name -> interp(init, env)))
    case Fun(param, ty, body) => CloV(param, body, env)
    case Id(x)                => env.getOrElse(x, error(s"free identifier"))
    case App(fun, arg) =>
      interp(fun, env) match
        case CloV(param, body, fenv) =>
          interp(body, fenv + (param -> interp(arg, env)))
        case _ => error(s"invalid operation")
}
