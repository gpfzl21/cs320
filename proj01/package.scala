package cs320

package object proj01 extends Project01 {

  // define several subfunctions

  def IntVOp(op: (Int, Int) => Int): (Value, Value) => Value
    = (_, _) match {
      case (IntV(x), IntV(y)) => IntV(op(x, y))
      case (x, y) => error(s"not both numbers: $x, $y")
      }

  val IntVAdd = IntVOp(_ + _)
  val IntVSub = IntVOp(_ - _)
  val IntVMul = IntVOp(_ * _)
  val IntVDiv = IntVOp(_ / _)   // add zero division

  def IntVOp_b(op: (Int, Int) => Boolean): (Value, Value) => Value
    = (_, _) match {
      case (IntV(x), IntV(y)) => BooleanV(op(x, y))
      case (x, y) => error(s"not both numbers: $x, $y")
      }

  val IntVEq = IntVOp_b(_ == _)
  val IntVLt = IntVOp_b(_ < _)

  def VType(v : Value): Type = v match {
    case IntV(n) => IntT
    case BooleanV(b) => BooleanT
    case TupleV(vs) => TupleT
    case NilV => ListT
    case ConsV(h, t) => ListT
    case CloV(_, _, _) => FunctionT
  }

  def FunDef_to_CloV(d: FunDef, env: Env): CloV = d match {
    case FunDef(n, ps, b) => CloV(ps, b, env)
    case _ => error(s"not a function definition: $d")
  }

  // define interpreter

  def interp(e: Expr, env: Env): Value = e match {
    case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
    case IntE(n) => IntV(n)
    case BooleanE(b) => BooleanV(b)
    case Add(l, r) => IntVAdd(interp(l, env), interp(r, env))
    case Mul(l, r) => IntVMul(interp(l, env), interp(r, env))
    case Div(l, r) =>
      val rv = interp(r, env)
      rv match {
        case IntV(0) => error(s"zero divisor")
        case _ => IntVDiv(interp(l, env), rv)
      }
    case Mod(l, r) =>
      val IntV(q_n) = interp(Div(l, r), env)
      val IntV(q_times_r_n) = interp(Mul(IntE(q_n), r), env)
      val IntV(neg_q_times_r_n) = interp(Mul(IntE(q_times_r_n), IntE(-1)), env)
      interp(Add(l, IntE(neg_q_times_r_n)), env)
      // actually it can be positive, but i follow scala's convention
    case Eq(l, r) => IntVEq(interp(l, env), interp(r, env))
    case Lt(l, r) => IntVLt(interp(l, env), interp(r, env))
    case If(c, t, f) => interp(c, env) match {
      case BooleanV(b) => b match {
        case true => interp(t, env)
        case false => interp(f, env)
      }
      case v => error(s"not a boolean: $v")
    }
    case TupleE(es) => es.length match {
      case 0 | 1 => error(s"length of $es is not greater than 1")
      case _ => TupleV(es.map(interp(_, env)))
    }
    case Proj(t, i) =>
      val tv = interp(t, env)
      tv match {
        case TupleV(vs) =>
          if (vs.length < i)
            error(s"too big index: $i is greater than length of $tv")
          else if
            (i <= 0) error(s"index not a natural number")
          else vs(i-1)
        case v => error(s"not a tuple: $v")
      }
    case NilE => NilV
    case ConsE(h, t) => interp(t, env) match {
      case NilV => ConsV(interp(h, env), NilV)
      case ConsV(nh, nt) => ConsV(interp(h, env), ConsV(nh, nt))
      case _ => error(s"not a list: $e")
    }
    case Empty(l) => interp(l, env) match {
      case NilV => BooleanV(true)
      case ConsV(h, t) => BooleanV(false)
      case v => error(s"not a list: $v")
    }
    case Head(l) => interp(l, env) match {
      case NilV => NilV
      case ConsV(h, t) => h
      case v => error(s"not a list: $v")
    }
    case Tail(l) => interp(l, env) match {
      case NilV => NilV
      case ConsV(h, t) => t
      case v => error(s"not a list: $v")
    }
    case Val(x, e, b) =>
      val v = interp(e, env)
      interp(b, env + (x -> v))
    case Fun(ps, b) => CloV(ps, b, env)
    case RecFuns(ds, b) =>
      val cloVs = ds.map(FunDef_to_CloV(_, env))
      val nenv = env ++ (ds.map(_.n) zip cloVs)
      cloVs.foreach(_.env = nenv)
      interp(b, nenv)
      // need to match cardinal number?
    case App(f, as) => interp(f, env) match {
      case CloV(ps, b, fenv) =>
        val avals = as.map(interp(_, env))
        if (avals.length != ps.length)
          error(s"wrong arity: ps is $ps, but avals is $avals")
        interp(b, fenv ++ (ps zip avals))
      case v => error(s"not a closure: $v")
    }
    case Test(e, t) =>
      if (VType(interp(e, env)) == t) BooleanV(true)
      else BooleanV(false)
  }




  def tests: Unit = {
    // test-int
    test(run("42"), "42")
    // test-add
    test(run("1 + 2"), "3")
    // test-sub
    test(run("7 - 2"), "5")
    // test-mul
    test(run("2 * 4"), "8")
    // test-div
    test(run("5 / 2"), "2")
    // test-mod
    test(run("13 % 5"), "3")
    // test-neg
    test(run("1 - -1"), "2")

    // test-boolean
    test(run("true"), "true")
    // test-eq
    test(run("1 == 3 - 2"), "true")
    // test-lt
    test(run("1 < 3 - 2"), "false")

    // test-tuple1
    test(run("(1, 2 + 3, true)"), "(1, 5, true)")
    // test-tuple2
    test(run("((42, 3 * 2), false)"), "((42, 6), false)")
    // test-proj1
    test(run("(1, 2 + 3, true)._1"), "1")
    // test-proj2
    test(run("((42, 3 * 2), false)._1._2"), "6")
    // test-proj4
//    test(run("(1, 2 + 3, true)._0"), "index is a natural number")

    // test-nil
    test(run("Nil"), "Nil")
    // test-cons
    test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
    // test-isempty1
    test(run("Nil.isEmpty"), "true")
    // test-isempty2
    test(run("(1 :: Nil).isEmpty"), "false")
    // test-isempty3
//    test(run("1.isEmpty"), "")
    // test-head
    test(run("(1 :: Nil).head"), "1")
    // test-tail
    test(run("(1 :: Nil).tail"), "Nil")
    // test-tail-head
    test(run("(1 :: 2 :: Nil).tail.head"), "2")
    
    // test-local1
    test(run("""
      val x = 1 + 2;
      val y = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-local2
    test(run("""
      val (x, y) = (1 + 2, 3 + 4);
      val z = x * y;
      val (a, b, c) = (z, z + z, z + z + z);
      c - b
    """), "21")

    // test-fun
    test(run("x => x + x"), "<function>")
    // test-app1
    test(run("(x => x + x)(1)"), "2")
    // test-app2
    test(run("(x => y => x + y)(1)(2)"), "3")
    // test-app3
    test(run("((x, y) => x + y)(1, 2)"), "3")

    // test-type1
    test(run("1.isInstanceOf[Int]"), "true")
    // test-type2
    test(run("1.isInstanceOf[Boolean]"), "false")
    // test-type3
    test(run("(1 :: Nil).isInstanceOf[List]"), "true")
    // test-type4
    test(run("(x => x + x).isInstanceOf[Function]"), "true")

    // test-if
    test(run("if (true) 1 else 2"), "1")
    // test-not
    test(run("!true"), "false")
    // test-and
    test(run("true && false"), "false")
    // test-or
    test(run("true || false"), "true")
    // test-neq
    test(run("1 != 2"), "true")
    // test-lte
    test(run("1 <= 1"), "true")
    // test-gt
    test(run("1 > 1"), "false")
    // test-gte
    test(run("1 >= 1"), "true")
    // test-nonempty
    test(run("Nil.nonEmpty"), "false")

    // test-rec1
    test(run("""
      def f(x) = x - 1;
      f(2)
    """), "1")
    // test-rec2
    test(run("""
      def f(x) = if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")


    // my test cases

    // test-id
    testExc(run("val x = 1; y"), "free identifier: y")
    test(run("val x = 8; x + 3"), "11")

    // test-arithmetic
    testExc(run("(x => x + x) + 3"),
      "not both numbers: <function>, 3")
    test(run("(x => x + 3)(2) + 5 + 7"), "17")
    test(run("val x = 7; x - (-5) + 2 * 2"), "16")
    test(run("(-1) + -3 * -7"), "20")
    test(run("10 + 5 / 3 - 3"), "8")
    testExc(run("10 + 5 / (3 - 3)"), "zero divisor")
    test(run("-21 % 5"), "-1")
    test(run("-21 % -5"), "-1")
    test(run("21 % -5"), "1")
    testExc(run("3 + 5 % (2 - 2)"), "zero divisor")

    // test-conditional arithmetic
    testExc(run("(1, 2) > 3"), "not both numbers: (1, 2), 3")
    test(run("-4 < 3"), "true")
    test(run("true && 5"), "5")
    testExc(run("2 || 1"), "not a boolean: 2")
    testExc(run("((x => x + 1), 6) || 3"),
      "not a boolean: (<function>, 6)")
    test(run("(1 < 2) || 6"), "true")
    test(run("(1 != (2 - 1)) || 4"), "4")

    // test-if statement
    testExc(run("if (1) 2 else 3"), "not a boolean: 1")
    test(run("if (1 == 2) 2 else 3"), "3")
    test(run("if (1 != 2) 2 else 3"), "2")
    testExc(run("if (val x = 1; x != 1) val x = 0; x else x + 1"),
      "free identifier: x")

    // test-sequence
    test(run("""val x = ((x => x + 1), 4 + 6, false);
          x._3 || (x._1(x._2) == 11)"""), "true")
    testExc(run("(1, 2, 3)._4"),
      "too big index: 4 is greater than length of (1, 2, 3)")
    testExc(run("(true :: 1 :: Nil)._1"),
      "not a tuple: (true :: (1 :: Nil))")

    // test-list
    test(run("2 :: 3 :: (x => (x + 1)) :: Nil"),
      "(2 :: (3 :: (<function> :: Nil)))")
    test(run("""
      val x = 2 :: 3 :: ((x, y) => (x + y)) :: Nil;
      x.head + x.tail.tail.head(3, 5) == 10
      """), "true")
    testExc(run("""
      val x = 2 :: 3 :: ((x, y) => (x + y)) :: Nil;
      x.head.isEmpty
      """), "not a list: 2")
    test(run("""
      val x = 2 :: 3 :: ((x, y) => (x + y)) :: Nil;
      x.tail.tail.tail.isEmpty
      """), "true")

    // test-def
    test(run("""
      def gcd(x, y) = if (y == 0) x else gcd(y, x % y);
      gcd(111, 259)
    """), "37")

    test(run("""
      def f(a) =
        if (a==0) 1
        else if (a==1) 2
        else if (a==2) 7
        else 2*f(a-1) + 3*f(a-2) + 3*f(a-3)+ 6*g(a-2) + 2*g(a-3) + 2*h(a-2);
        def g(b) =
        if (b==0) 0
        else if (b==1) 1
        else f(b-1)+g(b-1)+h(b-1);
        def h(c) =
        if (c==0) 0
        else if (c==1) 1
        else f(c-2)+g(c-1)+g(c-2);
        f(10) * g(3)
    """), "4835467")

    testExc(run("def f(a) = a + a; f(1, 2)"),
      "wrong arity: ps is List(a), but avals is List(1, 2)")
  }
}
