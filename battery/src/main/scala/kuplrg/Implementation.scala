package kuplrg

import javax.swing.text.StyledEditorKit.BoldAction
import scala.annotation.meta.param

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  // ---------------------------------------------------------------------------
  // typeCheck
  // ---------------------------------------------------------------------------

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match
    case EUnit    => UnitT
    case ENum(_)  => NumT
    case EBool(_) => BoolT
    case EStr(_)  => StrT
    case EId(x)   => tenv.vars.getOrElse(x, error(s"free identifier"))

    case EAdd(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT

    case EMul(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT

    case EDiv(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT

    case EMod(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      NumT

    case EConcat(left, right) =>
      mustSame(typeCheck(left, tenv), StrT)
      mustSame(typeCheck(right, tenv), StrT)
      StrT

    case EEq(left, right) =>
      val valt = typeCheck(left, tenv)
      mustSame(valt, typeCheck(right, tenv))
      BoolT

    case ELt(left, right) =>
      mustSame(typeCheck(left, tenv), NumT)
      mustSame(typeCheck(right, tenv), NumT)
      BoolT

    case ESeq(left, right) =>
      val t = typeCheck(left, tenv)
      typeCheck(right, tenv)

    case EIf(cond, thenExpr, elseExpr) =>
      mustSame(typeCheck(cond, tenv), BoolT)
      val valt = typeCheck(thenExpr, tenv)
      mustSame(valt, typeCheck(elseExpr, tenv))
      valt

    case EVal(x, tyOpt, expr, body) =>
      tyOpt match
        case None => typeCheck(body, tenv.addVar(x, typeCheck(expr, tenv)))
        case Some(valt) =>
          mustSame(valt, typeCheck(expr, tenv))
          typeCheck(body, tenv.addVar(x, valt))

    case EFun(params, body) =>
      ArrowT(Nil, (params.map(_.ty)).map(mustValid(_, tenv)),
          typeCheck(body,
              tenv.addVars(params.map(param => (param.name, param.ty)))))

    case EApp(fun, tys, args) =>
      val taus: List[Type] = tys.map(mustValid(_, tenv))
      typeCheck(fun, tenv) match
        case ArrowT(tvars, paramTys, retTy) =>
          if (tvars.length != tys.length || args.length != paramTys.length)
            error(s"parameter number mismatch")
          val tauuus: List[Type] = args.map(typeCheck(_, tenv))
          tauuus.zip(paramTys).map { case (tauuu, paramTy) =>
            mustSame(tauuu, subst(paramTy, tvars, taus))
          }
          subst(retTy, tvars, taus)
        case _ => error(s"EApp not ArrowT")

    case ERecDefs(defs, body) =>
      val newTEnv = tenvUpdate(tenv, defs)
      for (d <- defs) { ya(newTEnv, d) }
      mustValid(typeCheck(body, newTEnv), tenv)

    case EMatch(expr, mcases) =>
      typeCheck(expr, tenv) match
        case IdT(t, tys) =>
          tenv.tys.getOrElse(t, error(s"unknown type: $t")) match
            case TIAdt(tvars, variants) =>
              val xs = mcases.map(_.name).toSet
              if (variants.keySet != xs || xs.size != mcases.length)
                error("invalid case")
              val taus: List[Type] =
                mcases.map { case mcase =>
                  typeCheck(mcase.body,
                      casetenv(mcase, variants(mcase.name), tvars, tys, tenv))
                }
              taus.forall(isSame(taus.head, _)) match
                case true  => taus.head
                case false => error(s"different type")
            case _ => error(s"not t[tau]")
        case _ => error(s"not IdT")

    case EExit(ty, expr) =>
      typeCheck(expr, tenv) match
        case StrT => mustValid(ty, tenv)
        case _    => error(s"not exit")

  // ---------------------------------------------------------------------------
  // interp
  // ---------------------------------------------------------------------------

  def interp(expr: Expr, env: Env): Value = expr match
    case EUnit    => UnitV
    case ENum(n)  => NumV(n)
    case EBool(b) => BoolV(b)
    case EId(x) =>
      env.getOrElse(x, error(s"free identifier: $x")) match
        case ExprV(eexpr, eenv) => interp(eexpr, eenv())
        case x                  => x

    case EStr(s)           => StrV(s)
    case EAdd(left, right) => numAdd(interp(left, env), interp(right, env))
    case EMul(left, right) => numMul(interp(left, env), interp(right, env))
    case EDiv(left, right) => numDiv(interp(left, env), interp(right, env))
    case EMod(left, right) => numMod(interp(left, env), interp(right, env))
    case EEq(left, right)  => BoolV(eq(interp(left, env), interp(right, env)))
    case ELt(left, right)  => numLt(interp(left, env), interp(right, env))

    case EConcat(left, right) =>
      (interp(left, env), interp(right, env)) match
        case (StrV(left), StrV(right)) => StrV(left + right)
        case _                         => error(s"invalid operation: Concat")

    case ESeq(left, right) =>
      val v = interp(left, env)
      interp(right, env)

    case EIf(cond, thenExpr, elseExpr) =>
      interp(cond, env) match
        case BoolV(true)  => interp(thenExpr, env)
        case BoolV(false) => interp(elseExpr, env)
        case _            => error(s"invalid operation: If")

    case EVal(x, tyOpt, expr, body) =>
      tyOpt match
        case None     => interp(body, env + (x -> interp(expr, env)))
        case Some(ty) => interp(body, env + (x -> interp(expr, env)))

    case EFun(params, body) => CloV(params.map(_.name), body, () => env)

    case EApp(fun, tys, args) =>
      interp(fun, env) match
        case CloV(params, body, fenv) =>
          interp(body,
              fenv() ++ params.zip(args.map(interp(_, env))).map {
                case (params, args) => (params -> args)
              })
        case ConstrV(x) => VariantV(x, args.map(interp(_, env)))
        case _          => error(s"invalid operation not App")

    case ERecDefs(defs, body) =>
      lazy val finalenv: () => Env =
        defs.foldLeft(() => env) { (accenv, d) => () =>
          envUpdate(d, accenv(), finalenv)()
        }
      interp(body, finalenv())

    case EMatch(expr, mcases) =>
      interp(expr, env) match
        case VariantV(x, values) =>
          val matchcase = mcases.find(_.name == x)
          matchcase match
            case Some(MatchCase(x, params, body)) =>
              interp(body,
                  env ++ (params.zip(values).map) { case (param, value) =>
                    (param -> value)
                  })
            case None => error(s"no match cases")
        case _ => error(s"invalid operation not match")

    case _ => error(s"exit")

  // ---------------------------------------------------------------------------
  // Helper functions for interp
  // ---------------------------------------------------------------------------

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UnitV, UnitV)       => true
    case (NumV(l), NumV(r))   => l == r
    case (BoolV(l), BoolV(r)) => l == r
    case (StrV(l), StrV(r))   => l == r
    case (VariantV(l, lvs), VariantV(r, rvs)) =>
      l == r && lvs.zip(rvs).forall { case (lv, rv) => eq(lv, rv) }
    case (_, _) => false

  def numOp(op: (BigInt, BigInt) => BigInt)(l: Value, r: Value): Value =
    (l, r) match
      case (NumV(l), NumV(r)) => NumV(op(l, r))
      case _                  => error(s"invalid operation: +, *")

  def numOpDM(op: (BigInt, BigInt) => BigInt)(l: Value, r: Value): Value =
    (l, r) match
      case (NumV(l), NumV(r)) =>
        r match
          case 0 => error(s"invalid operation: /, %")
          case _ => NumV(op(l, r))
      case _ => error(s"invalid operation")

  def numOpLt(op: (BigInt, BigInt) => Boolean)(l: Value, r: Value): Value =
    (l, r) match
      case (NumV(l), NumV(r)) => BoolV(op(l, r))
      case _                  => error(s"invalid operation: <")

  val numAdd: (Value, Value) => Value = numOp(_ + _)
  val numMul: (Value, Value) => Value = numOp(_ * _)
  val numDiv: (Value, Value) => Value = numOpDM(_ / _)
  val numMod: (Value, Value) => Value = numOpDM(_ % _)
  val numLt: (Value, Value) => Value = numOpLt(_ < _)

  def envUpdate(d: RecDef, env: Env, finalenv: () => Env): () => Env =
    d match {
      case LazyVal(name, ty, init) =>
        () => env + (name -> ExprV(init, finalenv))
      case RecFun(name, tvars, params, rty, body) =>
        () =>
          env + (name -> CloV(params.map(param => param.name), body, finalenv))
      case TypeDef(name, tvars, varts) =>
        () =>
          env ++ (varts.flatMap(vart => Seq(vart.name -> ConstrV(vart.name))))
    }

  // ---------------------------------------------------------------------------
  // Helper functions for typeCheck
  // ---------------------------------------------------------------------------

  def mustValid(ty: Type, tenv: TypeEnv): Type = ty match
    case UnitT => UnitT
    case NumT  => NumT
    case BoolT => BoolT
    case StrT  => StrT

    case IdT(name, tys) =>
      tenv.tys.getOrElse(name, error(s"unkown type")) match
        case TIVar => IdT(name, tys)
        case TIAdt(tvars, variants) =>
          for (tau <- tys) {
            mustValid(tau, tenv)
          }
          IdT(name, tys)

    case ArrowT(tvars, paramTys, retTy) =>
      val newTEnv = tenv.addTypeVars(tvars)
      ArrowT(tvars, paramTys.map(mustValid(_, newTEnv)),
          mustValid(retTy, newTEnv))

  def substl(bodyTy: Type, name: String, ty: Type): Type = bodyTy match
    case UnitT => UnitT
    case NumT  => NumT
    case BoolT => BoolT
    case StrT  => StrT

    case IdT(x, tys) =>
      if (name == x) ty
      else IdT(x, tys.map(substl(_, name, ty)))

    case ArrowT(tvars, paramTys, retTy) =>
      ArrowT(tvars, paramTys.map(substl(_, name, ty)), substl(retTy, name, ty))

  def subst(bodyTy: Type, name: List[String], ty: List[Type]): Type = name match
    case Nil => bodyTy
    case _   => subst(substl(bodyTy, name.head, ty.head), name.tail, ty.tail)

  def isSame(lty: Type, rty: Type): Boolean = (lty, rty) match
    case (UnitT, UnitT) => true
    case (NumT, NumT)   => true
    case (BoolT, BoolT) => true
    case (StrT, StrT)   => true

    case (IdT(lalpha, Nil), IdT(ralpha, Nil)) =>
      if (lalpha == ralpha) true
      else false

    case (IdT(lt, ltys), IdT(rt, rtys)) =>
      lt == rt && ltys.zip(rtys).forall { case (lty, rty) => isSame(lty, rty) }

    case (ArrowT(ltvars, lparamTys, lretTy), ArrowT(rtvars, rparamTys,
                rretTy)) =>
      lparamTys
        .zip(rparamTys.map(subst(_, rtvars, ltvars.map(IdT(_)))))
        .forall { case (lty, rty) =>
          isSame(rty, lty)
        } && isSame(lretTy, subst(rretTy, rtvars, ltvars.map(IdT(_))))

    case _ => false

  def mustSame(lty: Type, rty: Type): Unit =
    if (!isSame(lty, rty)) error(s"type mismatch: ${lty.str} != ${rty.str}")

  def tenvUpdate(tenv: TypeEnv, d: List[RecDef]): TypeEnv = d match
    case Nil => tenv
    case _ =>
      d.head match
        case LazyVal(name, ty, init) =>
          tenvUpdate(tenv.addVar(name, ty), d.tail)

        case RecFun(name, tvars, params, rty, body) =>
          tenvUpdate(tenv.addVar(name, ArrowT(tvars, params.map(_.ty), rty)),
              d.tail)

        case TypeDef(name, tvars, varts) =>
          if (tenv.tys.contains(name)) error(s"already defined")
          val newTEnv = tenv.addTypeName(name, tvars, varts)
          val returnEnv = newTEnv.addVars(varts
                .map(_.name)
                .zip(varts.map(vart => ArrowT(tvars, vart.params.map(param => param.ty), IdT(name, tvars.map(IdT(_)))))))
          tenvUpdate(returnEnv, d.tail)

  def ya(tenv: TypeEnv, d: RecDef): Unit = d match
    case LazyVal(name, ty, init) =>
      mustSame(mustValid(ty, tenv), typeCheck(init, tenv))

    case RecFun(name, tvars, params, rty, body) =>
      for (tvar <- tvars) {
        if (tenv.tys.contains(tvar)) error(s"already defined")
      }
      val newTEnv = tenv.addTypeVars(tvars)
      mustSame(mustValid(rty, newTEnv),
          typeCheck(body,
              newTEnv.addVars(
                  params.map(param =>
                    (param.name, mustValid(param.ty, newTEnv))))))

    case TypeDef(name, tvars, varts) =>
      for (tvar <- tvars) {
        if (tenv.tys.contains(tvar)) error(s"already defined")
      }
      val newTEnv = tenv.addTypeVars(tvars)
      for (vart <- varts) {
        for (ty <- vart.params.map(_.ty)) {
          mustValid(ty, newTEnv)
        }
      }

  def casetenv(mcase: MatchCase, varparams: List[Param], tvars: List[String],
      tys: List[Type], tenv: TypeEnv): TypeEnv =
    tenv.addVars(
        mcase.params.zip(varparams.map(_.ty).map(subst(_, tvars, tys))))

}
