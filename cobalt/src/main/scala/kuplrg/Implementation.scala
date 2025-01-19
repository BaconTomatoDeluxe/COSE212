package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match
    case EUnit    => UnitV
    case ENum(n)  => NumV(n)
    case EBool(b) => BoolV(b)
    case EId(x)   => env.getOrElse(x, error(s"free identifier"))

    case EAdd(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => NumV(l + r)
        case _                  => error(s"invalid operation")

    case EMul(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => NumV(l * r)
        case _                  => error(s"invalid operation")

    case EDiv(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) =>
          r match
            case 0 => error(s"invalid operation")
            case _ => NumV(l / r)
        case (l, r) => error(s"invalid operation")

    case EMod(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) =>
          r match
            case 0 => error(s"invalid operation")
            case _ => NumV(l % r)
        case (l, r) => error(s"invalid operation")

    case EEq(l, r) => BoolV(eq(interp(l, env), interp(r, env)))

    case ELt(l, r) =>
      (interp(l, env), interp(r, env)) match
        case (NumV(l), NumV(r)) => BoolV(l < r)
        case (l, r)             => error(s"invalid operation")

    case EIf(c, t, e) =>
      interp(c, env) match
        case BoolV(true)  => interp(t, env)
        case BoolV(false) => interp(e, env)
        case v            => error(s"not a boolean")

    case ENil => NilV

    case ECons(h, t) =>
      interp(t, env) match
        case NilV              => ConsV(interp(h, env), NilV)
        case ConsV(head, tail) => ConsV(interp(h, env), ConsV(head, tail))
        case _                 => error(s"invalid operation")

    case EHead(l) =>
      interp(l, env) match
        case NilV        => error(s"empty list")
        case ConsV(h, t) => h
        case _           => error(s"not a list")

    case ETail(l) =>
      interp(l, env) match
        case NilV        => error(s"empty list")
        case ConsV(h, t) => t
        case _           => error(s"not a list")

    case ELength(l) => NumV(length(interp(l, env)))

    case EMap(l, f) => map(interp(l, env), interp(f, env))

    case EFlatMap(l, f) => join(map(interp(l, env), interp(f, env)))

    case EFilter(l, f) => filter(interp(l, env), interp(f, env))

    case ETuple(es) => TupleV(es.map(interp(_, env)))

    case EProj(t, i) =>
      interp(t, env) match
        case TupleV(vs) =>
          if (vs.length >= i) vs(i - 1) else error(s"out of bounds")
        case _ => error(s"not a tuple")

    case EVal(x, v, s) => interp(s, env + (x -> (interp(v, env))))

    case EFun(ps, b) => CloV(ps, b, () => env)

    case ERec(ds, s) =>
      lazy val newEnv: Env = env ++ ds.map { fdef =>
        fdef.name -> CloV(fdef.params, fdef.body, () => newEnv)
      }
      interp(s, newEnv)

    case EApp(f, as) => app(interp(f, env), as.map(interp(_, env)))

  def eq(left: Value, right: Value): Boolean = (left, right) match
    case (UnitV, UnitV)       => true
    case (NumV(l), NumV(r))   => l == r
    case (BoolV(l), BoolV(r)) => l == r
    case (NilV, NilV)         => true
    case (ConsV(l_h, l_t), ConsV(r_h, r_t)) =>
      eq(l_h, r_h) && eq(l_t, r_t)
    case (NilV, ConsV(h, t)) => false
    case (ConsV(h, t), NilV) => false
    case (TupleV(l), TupleV(r)) =>
      (l.length == r.length) && l.zip(r).forall { case (vl, vr) => eq(vl, vr) }
    case _ => error(s"invalid operation")

  def length(list: Value): BigInt = list match
    case NilV            => 0
    case ConsV(v_h, v_t) => 1 + length(v_t)
    case v               => error(s"not a list")

  def map(list: Value, fun: Value): Value = list match
    case NilV        => NilV
    case ConsV(h, t) => ConsV(app(fun, h :: Nil), map(t, fun))
    case _           => error(s"not a list")

  def join(list: Value): Value = list match
    case NilV => NilV
    case ConsV(h, t) =>
      h match
        case NilV              => join(t)
        case ConsV(head, tail) => ConsV(head, join(ConsV(tail, t)))
        case _                 => error(s"not a list")
    case _ => error(s"not a list")

  def filter(list: Value, fun: Value): Value = list match
    case NilV => NilV
    case ConsV(h, t) =>
      app(fun, h :: Nil) match
        case BoolV(true)  => ConsV(h, filter(t, fun))
        case BoolV(false) => filter(t, fun)
        case _            => error(s"not a boolean")
    case _ => error(s"not a list")

  def app(fun: Value, args: List[Value]): Value = fun match
    case CloV(ps, b, env) =>
      interp(b, env() ++ ps.zip(args.padTo(ps.length, UnitV)))
    case _ => error(s"not a function")
}
