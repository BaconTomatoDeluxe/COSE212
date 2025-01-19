package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case Num(number) => NumT
    case Add(left, right) =>
  

  def interp(expr: Expr, env: Env): Value = ???

  def subtype(l: Type, r: Type): Unit = (l, r) match
    case (BotT, t) => Unit
    case (t, TopT) => Unit
    case (l, r) => if (l == r) Unit
    case (ArrowT(lpt, lrt), ArrowT(rpt, rrt)) =>
      subtype(rpt, lpt)
      subtype(rrt, lrt)
    case (RecordT(lfs), RecordT(rfs)) =>
      
  

  
}
