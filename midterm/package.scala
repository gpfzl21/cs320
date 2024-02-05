package cs320

package object midterm extends Midterm {
  // subfunctions
  def numVop(op: (Int, Int) => Int): (Value, Value) => NumV = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"impossible operands: $x, $y")
  }
  val numVAdd = numVop(_ + _)
  val numVSub = numVop(_ - _)

  def extract_V(x: (Value, Sto)): Value = x match {
    case (v, s) => v
  }

  // allocate new address
  def malloc(sto: Sto): Addr = (-1 :: sto.keys.toList).max + 1

  // interpreter
  def interp(expr: Expr, env: Env, stru: Stru, op: Oper, sto: Sto): (Value, Sto) = expr match {
    case Num(n) => (NumV(n), sto)
    case Add(l, r) =>
      val (lv, ls) = interp(l, env, stru, op, sto)
      val (rv, rs) = interp(r, env, stru, op, ls)
      (lv, rv) match {
        case (RecV(x, f), _) =>
          val overop = op.getOrElse((x, "+"), error(s"impossible operands: $lv, $rv"))
          overop match {
            case CloV(ps, e, fenv) =>
              // dont need to confrim ps length by OverAdd
              interp(e, fenv ++ (ps zip List(lv, rv)), stru, op, rs)
          }
        case (_, RecV(x, f)) =>
          val overop = op.getOrElse((x, "+"), error(s"impossible operands: $lv, $rv"))
          overop match {
            case CloV(ps, e, fenv) =>
              // dont need to confrim ps length by OverAdd
              interp(e, fenv ++ (ps zip List(lv, rv)), stru, op, rs)
          }
        case (_, _) => (numVAdd(lv, rv), rs)
      }
    case Sub(l, r) =>
      val (lv, ls) = interp(l, env, stru, op, sto)
      val (rv, rs) = interp(r, env, stru, op, ls)
      (lv, rv) match {
        case (RecV(x, f), _) =>
          val overop = op.getOrElse((x, "-"), error(s"impossible operands: $lv, $rv"))
          overop match {
            case CloV(ps, e, fenv) =>
              // dont need to confrim ps length by OverAdd
              interp(e, fenv ++ (ps zip List(lv, rv)), stru, op, rs)
          }
        case (_, RecV(x, f)) =>
          val overop = op.getOrElse((x, "-"), error(s"impossible operands: $lv, $rv"))
          overop match {
            case CloV(ps, e, fenv) =>
              // dont need to confrim ps length by OverAdd
              interp(e, fenv ++ (ps zip List(lv, rv)), stru, op, rs)
          }
        case (_, _) => (numVSub(lv, rv), rs)
      }
    case Id(x) =>
      val v = env.getOrElse(x, error(s"free identifier: $x"))
      (v, sto)
    case Val(x, e, b) =>
      val (v, vs) = interp(e, env, stru, op, sto)
      interp(b, env + (x -> v), stru, op, vs)
    case Fun(ps, b) => (CloV(ps, b, env), sto)
    case App(f, args) =>
      val (fv, fs) = interp(f, env, stru, op, sto)
      fv match {
        case CloV(ps, b, fenv) =>
          val (avals, s) = args.foldLeft(List[Value](), fs) {
            case ((list_val, s0), e) =>
              val (v, s1) = interp(e, env, stru, op, s0)
              (list_val ::: List(v), s1)
          }
          if (args.length != ps.length)
            error(s"wrong arity: $ps, $args")
          interp(b, fenv ++ (ps zip avals), stru, op, s)
          // args: List[Expr]
        case v => error(s"not a closure: $v")
      }
    case NewBox(e) =>
      val (v, s) = interp(e, env, stru, op, sto)
      val addr = malloc(s)
      (BoxV(addr), s + (addr -> v))
    case SetBox(b, e) =>
      val (bv, bs) = interp(b, env, stru, op, sto)
      bv match {
        case BoxV(addr) =>
          val (v, s) = interp(e, env, stru, op, bs)
          (v, s + (addr -> v))
        case v => error(s"not a box: $v")
      }
    case OpenBox(b) =>
      val (bv, bs) = interp(b, env, stru, op, sto)
      bv match {
        case BoxV(addr) =>
          val v = bs.getOrElse(addr, error(s"unallocated address: $addr"))
          (v, bs)
        case v => error(s"not a box: $v")
      }
    case Seqn(l, rs) =>
      val initial = interp(l, env, stru, op, sto)
      rs.foldLeft(initial) {
        case ((v, s), r) => interp(r, env, stru, op, s)
      }
    case Struct(x, fs, e) => stru.get(x) match {
      case None => interp(e, env, stru + (x -> fs), op, sto)
      case Some(_) => error(s"duplicated struct: $x")
    }
//case class Struct(name: String, fields: List[String], e: Expr)
    case Rec(x, fs) =>
      val struct_fs = stru.getOrElse(x, error(s"no such struct: $x"))
      val input_fs = fs.map{case (s, e) => s}
      if (struct_fs != input_fs)
        error(s"not same fields: $struct_fs & $input_fs")
      val (fields, s) = fs.foldLeft(Map[String, Addr](), sto) {
        case ((m0, s0), (f, e)) =>
          val (v, s1) = interp(e, env, stru, op, s0)
          val addr = malloc(s1)
          val s2 = s1 + (addr -> v)
          val m1 = m0 + (f -> addr)
          (m1, s2)
      }
      (RecV(x, fields), s)
    case Get(r, f) =>
      val (rv, rs) = interp(r, env, stru, op, sto)
      rv match {
        case RecV(x, fs) =>
          val addr = fs.getOrElse(f, error(s"no such field: $f"))
          val v = rs.getOrElse(addr, error(s"unallocated address: $addr"))
          (v, rs)
        case v => error(s"not a record: $v")
      }
    case Set(r, f, e) =>
      val (rv, rs) = interp(r, env, stru, op, sto)
      rv match {
        case RecV(x, fs) =>
          val addr = fs.getOrElse(f, error(s"no such field: $f"))
          val (v, s) = interp(e, env, stru, op, rs)
          (v, s + (addr -> v))
        case v => error(s"not a record: $v")
      }
    case OverAdd(x, overop, e) =>
      val struct_fs = stru.getOrElse(x, error(s"no such struct: $x"))
      val (overopv, overs) = interp(overop, env, stru, op, sto)
      overopv match {
        case CloV(ps, b, fenv) =>
          if (ps.length != 2)
            error(s"not binary: $overopv")
          interp(e, env, stru, op + ((x, "+") -> overopv), overs)
        case _ => error(s"not operator: $overopv")
      }
//case class OverAdd(struct: String, oper: Expr, e: Expr)
    case OverSub(x, overop, e) =>
      val struct_fs = stru.getOrElse(x, error(s"no such struct: $x"))
      val (overopv, overs) = interp(overop, env, stru, op, sto)
      overopv match {
        case CloV(ps, b, fenv) =>
          if (ps.length != 2)
            error(s"not binary: $overopv")
          interp(e, env, stru, op + ((x, "-") -> overopv), overs)
        case _ => error(s"not operator: $overopv")
      }
  }
  def tests: Unit = {
    test(extract_V(interp(
      Struct("Vector", List("x", "y"),
        OverAdd("Vector",
        Fun(List("a", "b"), Rec("Vector", List(
          ("x", Add(Get(Id("a"), "x"), Get(Id("b"), "x"))),
          ("y", Add(Get(Id("a"), "y"), Get(Id("b"), "y")))
        ))),
          Val("p", Rec("Vector", List(("x", Num(1)), ("y", Num(2)))),
            Val("q", Rec("Vector", List(("x", Num(3)), ("y", Num(4)))),
              Val("r", Rec("Vector", List(("x", Num(5)), ("y", Num(6)))),
                Val("sum_of_pqr", Add(Add(Id("p"), Id("q")), Id("r")),
                  Sub(
                    Add(Get(Id("sum_of_pqr"), "x"), Get(Id("sum_of_pqr"), "x")),
                    Get(Id("sum_of_pqr"), "y")
                  )
                )
              )
            )
          )
        )
      )
    , Map(), Map(), Map(), Map())), NumV(6))
    test(extract_V(interp(
      Struct("Vector", List("x", "y"),
        OverAdd("Vector",
        Fun(List("a", "b"), Rec("Vector", List(
          ("x", Add(Get(Id("a"), "x"), Id("b"))),
          ("y", Add(Get(Id("a"), "y"), Id("b")))
        ))),
          Val("p", Rec("Vector", List(("x", Num(1)), ("y", Num(2)))),
            Val("q", Num(3),
              Val("r", Num(4),
                Val("sum_of_pqr", Add(Add(Id("p"), Id("q")), Id("r")),
                  Sub(
                    Add(Get(Id("sum_of_pqr"), "x"), Get(Id("sum_of_pqr"), "x")),
                    Get(Id("sum_of_pqr"), "y")
                  )
                )
              )
            )
          )
        )
      )
    , Map(), Map(), Map(), Map())), NumV(7))
  }
}




























