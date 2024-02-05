package cs320

package object ex06 extends Exercise06 {
  // operators for NumV values
  def numVop(op: (Int, Int) => Int): (Value, Value) => NumV = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numVAdd = numVop(_ + _)
  val numVSub = numVop(_ - _)

  // interpreter
  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = e match {
    case Num(n) => (NumV(n), sto)
    case Add(l, r) =>
      val (lv, ls) = interp(l, env, sto)
      val (rv, rs) = interp(r, env, ls)
      (numVAdd(lv, rv), rs)
    case Sub(l, r) =>
      val (lv, ls) = interp(l, env, sto)
      val (rv, rs) = interp(r, env, ls)
      (numVSub(lv, rv), rs)
    case Id(x) =>
      val v = env.getOrElse(x, error(s"free identifier: $x"))
      (v, sto)
    case Fun(p, b) =>
      val cloV = CloV(p, b, env)
      (cloV, sto)
    case App(f, arg) =>
      val (fv, fs) = interp(f, env, sto)
      fv match {
        case CloV(param, b, fenv) =>
          val (av, as) = interp(arg, env, fs)
          interp(b, fenv + (param -> av), as)
        case _ => error(s"not a closure: $fv")
      }
//      val (fv, fs) = interp(f, env, sto)
//      val CloV(param, b, fenv) = cast[CloV](fv, s"not a closure: $fv")
//      val (av, as) = interp(arg, env, fs)
//      interp(b, fenv + (param -> av), as)
    case NewBox(e) =>
      val (v, s) = interp(e, env, sto)
      val addr = malloc(s)
      (BoxV(addr), s + (addr -> v))
    case SetBox(b, e) =>
      val (bv, bs) = interp(b, env, sto)
      val BoxV(addr) = cast[BoxV](bv, s"not a box: $bv")
      val (v, s) = interp(e, env, bs)
      (v, s + (addr -> v))
    case OpenBox(b) =>
      val (bv, bs) = interp(b, env, sto)
      val BoxV(addr) = cast[BoxV](bv, s"not a box: $bv")
      val v = bs.getOrElse(addr, error(s"unallocated address: $addr"))
      (v, bs)
    case Seqn(l, rs) =>
      val (lv, ls) = interp(l, env, sto)
      rs match {
        case h :: t =>
          interp(Seqn(h, t), env, ls)
        case Nil => (lv, ls)
      }
//      val initial = interp(l, env, sto)
//      rs.foldLeft(initial) {
//        case ((v, s), r) => interp(r, env, s)
//      }
    case Rec(fs) =>
      val (fields, s) = fs.foldLeft(Map[String, Addr](), sto) {
        case ((m0, s0), (f, e)) =>
          val (v, s1) = interp(e, env, s0)
          val addr = malloc(s1)
          val s2 = s1 + (addr -> v)
          val m1 = m0 + (f -> addr)
          (m1, s2)
      }
      (RecV(fields), s)
    case Get(r, f) =>
      val (rv, rs) = interp(r, env, sto)
      val RecV(fields) = cast[RecV](rv, s"not a record: $rv")
      val addr = fields.getOrElse(f, error(s"no such field: $f"))
      val v = rs.getOrElse(addr, error(s"unallocated address: $addr"))
      (v, rs)
    case Set(r, f, e) =>
      val (rv, rs) = interp(r, env, sto)
      val RecV(fields) = cast[RecV](rv, s"not a record: $rv")
      val addr = fields.getOrElse(f, error(s"no such field: $f"))
      val (v, s) = interp(e, env, rs)
      (v, s + (addr -> v))
  }

  // allocate new address
  def malloc(sto: Sto): Addr = (-1 :: sto.keys.toList).max + 1

  def tests: Unit = {
    test(run("{ 1; 2 }"), "2")
    test(run("{ b => b.get }(Box(10))"), "10")
    test(run("{ b => { b.set(12); b.get } }(Box(10))"), "12")
    test(run("{ b => b.get }({ Box(9); Box(10) })"), "10")
    test(run("{ b => { a => b.get } }(Box(9))(Box(10))"), "9")
    test(run("{ b => { b.set(2); b.get } }(Box(1))"), "2")
    test(run("{ b => { b.set((9 + b.get)); b.get } }(Box(1))"), "10")
    test(run("{ b => { b.set((2 + b.get)); b.set((3 + b.get)); b.set((4 + b.get)); b.get } }(Box(1))"), "10")
    test(run("{ r => r.x }({ x = 1 })"), "1")
    test(run("{ r => { { r.x = 5 }; r.x } }({ x = 1 })"), "5")
    test(run("{ g => { s => { r1 => { r2 => (r1.b + { s(r1)(g(r2)); ({ s(r2)(g(r1)); r1.b } + r2.b) }) } } } }({ r => r.a })({ r => { v => { r.b = v } } })({ a = 0, b = 2 })({ a = 3, b = 4 })"), "5")
    test(run("{ x => x }"), "function")
    test(run("Box(1)"), "box")
    test(run("{}"), "record")
  }
}
