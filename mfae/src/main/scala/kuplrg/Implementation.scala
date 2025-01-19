package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match
    case Num(n)   => (NumV(n), mem)
    case Id(name) => (mem(env.getOrElse(name, error(s"free identifier"))), mem)

    case Add(l, r) =>
      val (lv, m1) = interp(l, env, mem)
      val (rv, m2) = interp(r, env, m1)
      (lv, rv) match
        case (NumV(l), NumV(r)) => (NumV(l + r), m2)
        case _                  => error(s"invalid operation")

    case Mul(l, r) =>
      val (lv, m1) = interp(l, env, mem)
      val (rv, m2) = interp(r, env, m1)
      (lv, rv) match
        case (NumV(l), NumV(r)) => (NumV(l * r), m2)
        case _                  => error(s"invalid operation")

    case Var(n, init, b) =>
      val (iv, m1) = interp(init, env, mem)
      val addr = malloc(m1)
      interp(b, env + (n -> addr), m1 + (addr -> iv))

    case Fun(p, b) => (CloV(p, b, env), mem)

    case App(f, arg) =>
      val (fv, m1) = interp(f, env, mem)
      fv match
        case CloV(p, b, fenv) =>
          val (v2, m2) = interp(arg, env, m1)
          val addr = malloc(m2)
          interp(b, fenv + (p -> addr), m2 + (addr -> v2))
        case _ => error(s"not a function")

    case Assign(n, e) =>
      val (ev, em) = interp(e, env, mem)
      (ev, em + (env.getOrElse(n, error(s"free identifier")) -> ev))

    case Seq(l, r) =>
      val (lv, m1) = interp(l, env, mem)
      interp(r, env, m1)

  def interpCBR(expr: Expr, env: Env, mem: Mem): (Value, Mem) = expr match
    case Num(n)   => (NumV(n), mem)
    case Id(name) => (mem(env.getOrElse(name, error(s"free identifier"))), mem)

    case Add(l, r) =>
      val (lv, m1) = interpCBR(l, env, mem)
      val (rv, m2) = interpCBR(r, env, m1)
      (lv, rv) match
        case (NumV(l), NumV(r)) => (NumV(l + r), m2)
        case _                  => error(s"invalid operation")

    case Mul(l, r) =>
      val (lv, m1) = interpCBR(l, env, mem)
      val (rv, m2) = interpCBR(r, env, m1)
      (lv, rv) match
        case (NumV(l), NumV(r)) => (NumV(l * r), m2)
        case _                  => error(s"invalid operation")

    case Var(n, init, b) =>
      val (iv, m1) = interpCBR(init, env, mem)
      val addr = malloc(m1)
      interpCBR(b, env + (n -> addr), m1 + (addr -> iv))

    case Fun(p, b) => (CloV(p, b, env), mem)

    case App(f, arg) =>
      val (fv, m1) = interpCBR(f, env, mem)
      fv match
        case CloV(p, b, fenv) =>
          val (v2, m2) = interpCBR(arg, env, m1)
          arg match
            case Id(n) =>
              interpCBR(
                b,
                fenv + (p -> env.getOrElse(n, error(s"free identifier"))),
                m1
              )
            case _ =>
              val addr = malloc(m2)
              interpCBR(b, fenv + (p -> addr), m2 + (addr -> v2))
        case _ => error(s"not a function")

    case Assign(n, e) =>
      val (ev, em) = interpCBR(e, env, mem)
      (ev, em + (env.getOrElse(n, error(s"free identifier")) -> ev))

    case Seq(l, r) =>
      val (lv, m1) = interpCBR(l, env, mem)
      interpCBR(r, env, m1)
}
