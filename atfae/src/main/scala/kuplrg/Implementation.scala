package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(number) => NumT
    case Bool(bool)  => BoolT
    case Add(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Mul(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Div(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Mod(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT
    case Eq(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      BoolT
    case Lt(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      BoolT
    case Val(name, init, body) =>
      typeCheck(body, tenv + (name -> typeCheck(init, tenv)))
    case Id(x) => tenv.getOrElse(x, error(s"free identifier"))
    case Fun(params, body) =>
      val taus = params.map(typeCheck(_, tenv))
      val tau = typeCheck(body,
          tenv ++ params.zip(taus).map { case (param, t) => param -> t })
      ArrowT(taus, tau)
    case Rec(x, params, rty, body, scope) =>
      val taus = params.map(typeCheck(_, tenv))
      val tau = typeCheck(body, tenv + )

  def interp(expr: Expr, env: Env): Value = ???

  def isSame(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (NumT, MumT)   => true
    case (BoolT, BoolT) => true
    case (ArrowT(lparamTys, lretTy), ArrowT(rparamTys, rretTy)) =>
      lretTy == rretTy && lparamTys.zip(rretTy).forall { case (lty, rty) =>
        lty == rty
      }
    case (NameT, NameT) => true
    case _              => false

  def mustSame(lty: Type, rty: Type): Unit =
    if (!(isSame(lty, rty))) error(s"type mismatch")

  def isValid()
}
