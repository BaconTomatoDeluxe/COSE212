package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Inst.*
  import Control.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def reduce(st: State): State =
    val State(k, s, h, mem) = st
    (k, s) match
      // 4.1 Reduction Relations for IEval
      case (IEval(env, expr) :: k, s) =>
        expr match
          case EUndef   => State(k, UndefV :: s, h, mem)
          case ENum(n)  => State(k, NumV(n) :: s, h, mem)
          case EBool(b) => State(k, BoolV(b) :: s, h, mem)
          case EAdd(l, r) =>
            State(IEval(env, l) :: IEval(env, r) :: IAdd :: k, s, h, mem)
          case EMul(l, r) =>
            State(IEval(env, l) :: IEval(env, r) :: IMul :: k, s, h, mem)
          case EDiv(l, r) =>
            State(IEval(env, l) :: IEval(env, r) :: IDiv :: k, s, h, mem)
          case EMod(l, r) =>
            State(IEval(env, l) :: IEval(env, r) :: IMod :: k, s, h, mem)
          case EEq(l, r) =>
            State(IEval(env, l) :: IEval(env, r) :: IEq :: k, s, h, mem)
          case ELt(l, r) =>
            State(IEval(env, l) :: IEval(env, r) :: ILt :: k, s, h, mem)
          case EVar(x, i, b) =>
            State(IEval(env, i) :: IDef(List(x), env, b) :: k, s, h, mem)
          case EId(x) => State(k, mem(lookup(env, x)) :: s, h, mem)
          case EAssign(x, e) =>
            State(IEval(env, e) :: IWrite(lookup(env, x)) :: k, s, h, mem)
          case ESeq(l, r) =>
            State(IEval(env, l) :: IPop :: IEval(env, r) :: k, s, h, mem)

          // 4.1.1 Conditionals and While Loops
          case EIf(c, t, e) =>
            State(IEval(env, c) :: IJmpIf(KValue(IEval(env, t) :: k, s,
                        h)) :: IEval(env, e) :: k, s, h, mem)
          case EWhile(c, b) =>
            val kc: KValue = KValue(IPop :: IEval(env, EWhile(c, b)) :: k, s, h)
            val kb: KValue = KValue(k, s, h)
            val hb: Handler = h ++ Map(Continue -> kc, Break -> kb)
            val kbody: KValue =
              KValue(IEval(env, b) :: IJmp(Continue) :: Nil, s, hb)
            State(IEval(env, c) :: IJmpIf(kbody) :: k, UndefV :: s, h, mem)

          case EBreak => State(IJmp(Break) :: Nil, UndefV :: s, h, mem)

          case EContinue => State(IJmp(Continue) :: Nil, UndefV :: s, h, mem)

          // 4.1.2 Functions and Return
          case EFun(ps, b) => State(k, CloV(ps, b, env) :: s, h, mem)
          case EApp(f, args) =>
            State(IEval(env, f) :: args.map(arg => IEval(env, arg)) ::: ICall(
                    args.length) :: k, s, h, mem)
          case EReturn(e) => State(IEval(env, e) :: IReturn :: k, s, h, mem)

          // 4.1.3 Exceptions
          case ETry(b, cp, ce) =>
            val pt = KValue(IDef(List(cp), env, ce) :: k, s, h)
            val pf = KValue(k, s, h)
            val hb = h + (Throw -> pt, Finally -> pf)
            State(IEval(env, b) :: IJmp(Finally) :: Nil, s, hb, mem)

          case EThrow(e) =>
            State(IEval(env, e) :: IJmp(Throw) :: Nil, s, h, mem)

          // 4.1.4 Generators
          case EGen(ps, b) => State(k, GenV(ps, b, env) :: s, h, mem)
          case EIterNext(iter, a) =>
            a match
              case None =>
                State(IEval(env, iter) :: IEval(env, EUndef) :: INext :: k, s,
                    h, mem)
              case Some(a) =>
                State(IEval(env, iter) :: IEval(env, a) :: INext :: k, s, h,
                    mem)
          case EYield(e) =>
            val pn = KValue(k, s, h)
            State(IEval(env, e) :: IYield :: Nil,
                BoolV(false) :: ContV(pn) :: s, h, mem)
          case EValueField(r) =>
            State(IEval(env, r) :: IValueField :: k, s, h, mem)
          case EDoneField(r) =>
            State(IEval(env, r) :: IDoneField :: k, s, h, mem)

      // 4.2 Reduction Relations for Other Instructions
      case (IAdd :: k, r :: l :: s) => State(k, numAdd(l, r) :: s, h, mem)
      case (IMul :: k, r :: l :: s) => State(k, numMul(l, r) :: s, h, mem)
      case (IDiv :: k, r :: l :: s) => State(k, numDiv(l, r) :: s, h, mem)
      case (IMod :: k, r :: l :: s) => State(k, numMod(l, r) :: s, h, mem)
      case (IEq :: k, r :: l :: s)  => State(k, BoolV(eq(l, r)) :: s, h, mem)
      case (ILt :: k, r :: l :: s)  => State(k, numLt(l, r) :: s, h, mem)

      case (IDef(xs, env, b) :: k, s) =>
        val addrs = malloc(mem, xs.length)
        val (vs, sr) = s.splitAt(xs.length)
        State(IEval(env ++ (xs.zip(addrs).toMap), b) :: k, sr, h,
            mem ++ (addrs.zip(vs.reverse).toMap))

      case (IWrite(a) :: k, v :: s) =>
        State(k, v :: s, h, mem + (a -> v))
      case (IPop :: k, v :: s) => State(k, s, h, mem)

      // 4.2.1 Control Flow Instructions
      case (IJmpIf(ks) :: k, BoolV(b) :: s) =>
        b match
          case true =>
            ks match
              case KValue(ck, cs, ch) => State(ck, cs, ch, mem)
          case false => State(k, s, h, mem)

      case (IJmp(c) :: k, v :: s) =>
        val KValue(k1, s1, h1) = lookup(h, c)
        val h2 = h.contains(Yield) match
          case true  => h1 + (Yield -> lookup(h, Yield))
          case false => h1
        State(k1, v :: s1, h2, mem)

      // 4.2.2 Function Call/Return Instructions
      case (ICall(n) :: k, s) =>
        val (sb, rest) = s.splitAt(n)
        val cg :: sr = rest
        cg match
          case CloV(ps, b, fenv) =>
            val cr = KValue(k, sr, h)
            val hb = h + (Return -> cr) - Break - Continue - Yield
            val m = ps.length
            n >= m match
              case true =>
                State(IDef(ps, fenv, EReturn(b)) :: Nil, sb.drop(n - m), hb,
                    mem)
              case false =>
                State(IDef(ps, fenv, EReturn(b)) :: Nil,
                    List.fill(m - n)(UndefV) ++ sb, hb, mem)

          case GenV(ps, b, genv) =>
            val a = malloc(mem)
            val m = ps.length
            val sbody = if (n >= m) { sb.drop(n - m) }
            else { List.fill(m - n)(UndefV) ++ sb }
            val kb =
              IPop :: IDef(ps, genv, EReturn(ETry(b, "x", EId("x")))) :: Nil
            val pb = ContV(KValue(kb, sbody, Map.empty))
            State(k, IterV(a) :: sr, h, mem + (a -> pb))

          case _ => error(s"invalid operation")

      case (IReturn :: k, v :: s) =>
        val pd = ContV(KValue(IReturn :: Nil, Nil, Map.empty))
        h.contains(Yield) match
          case true => State(IYield :: Nil, v :: BoolV(true) :: pd :: s, h, mem)
          case false =>
            h.contains(Return) match
              case true  => State(IJmp(Return) :: Nil, v :: Nil, h, mem)
              case false => error(s"invalid operation")

      // 4.2.3 Generator Instructions
      case (INext :: k, v :: IterV(a) :: s) =>
        val p = KValue(k, IterV(a) :: s, h)
        val ContV(KValue(k1, s1, h1)) = mem(a)
        val hb = h1 + (Yield -> p, Return -> p)
        State(k1, v :: s1, hb, mem)

      case (IYield :: _, v :: BoolV(b) :: v1 :: _) =>
        val KValue(k1, IterV(a) :: s1, h1) = lookup(h, Yield)
        State(k1, ResultV(v, b) :: s1, h1, mem + (a -> v1))

      case (IValueField :: k, ResultV(v, _) :: s) => State(k, v :: s, h, mem)
      case (IDoneField :: k, ResultV(_, b) :: s) =>
        State(k, BoolV(b) :: s, h, mem)

      case _ => error(s"invalid operation")

  // ---------------------------- -----------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def bodyOfSquares: String = s"""
      function* range(begin, end) {
        if (end == undefined) { end = begin; begin = 0; }
        else undefined;
        while (begin <= end) yield begin++;
      }

      for (i of range(from, to)) yield i * i;
      """

  // ---------------------------------------------------------------------------
  // Helper functions
  // ---------------------------------------------------------------------------
  def malloc(mem: Mem, n: Int): List[Addr] =
    val a = malloc(mem)
    (0 until n).toList.map(a + _)

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

  def lookup(env: Env, x: String): Addr =
    env.getOrElse(x, error(s"free identifier: $x"))

  def lookup(handler: Handler, x: Control): KValue =
    handler.getOrElse(x, error(s"invalid control operation: $x"))

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UndefV, UndefV)                   => true
    case (NumV(l), NumV(r))                 => l == r
    case (BoolV(l), BoolV(r))               => l == r
    case (IterV(l), IterV(r))               => l == r
    case (ResultV(lv, ld), ResultV(rv, rd)) => eq(lv, rv) && ld == rd
    case _                                  => false

  def numOp(op: (BigInt, BigInt) => BigInt)(l: Value, r: Value): Value =
    (l, r) match
      case (NumV(l), NumV(r)) => NumV(op(l, r))
      case _                  => error(s"invalid operation")

  def numOpDM(op: (BigInt, BigInt) => BigInt)(l: Value, r: Value): Value =
    (l, r) match
      case (NumV(l), NumV(r)) =>
        r match
          case 0 => error(s"invalid operation")
          case _ => NumV(op(l, r))
      case _ => error(s"invalid operation")

  def numOpLt(op: (BigInt, BigInt) => Boolean)(l: Value, r: Value): Value =
    (l, r) match
      case (NumV(l), NumV(r)) => BoolV(op(l, r))
      case _                  => error(s"invalid operation")

  val numAdd: (Value, Value) => Value = numOp(_ + _)
  val numMul: (Value, Value) => Value = numOp(_ * _)
  val numDiv: (Value, Value) => Value = numOpDM(_ / _)
  val numMod: (Value, Value) => Value = numOpDM(_ % _)
  val numLt: (Value, Value) => Value = numOpLt(_ < _)

}
