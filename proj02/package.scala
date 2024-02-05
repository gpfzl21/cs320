package cs320

package object proj02 extends Project02 {

  def IntVOp(op: (Int, Int) => Int): (Value, Value) => Value = (_, _) match {
      case (IntV(x), IntV(y)) => IntV(op(x, y))
      case (x, y)
        => error(s"not both numbers: $x, $y")
  }

  val IntVAdd = IntVOp(_ + _)
  val IntVMul = IntVOp(_ * _)

  def IntVOp2(op: (Int, Int) => Int): (Value, Value) => Value = (_, _) match {
      case (IntV(x), IntV(y)) =>
        if (y==0) error(s"zero divisor")
        IntV(op(x, y))
      case (x, y) => error(s"not both numbers: $x, $y")
  }

  val IntVDiv = IntVOp2(_ / _)
  val IntVMod = IntVOp2(_ % _)

  def IntVOp3(op: (Int, Int) => Boolean): (Value, Value) => Value = (_, _) match {
    case (IntV(x), IntV(y)) => BooleanV(op(x, y))
    case (x, y)
      => error(s"not both numbers: $x, $y")
  }

  val IntVEq = IntVOp3(_ == _)
  val IntVLt = IntVOp3(_ < _)

  def FunDef_to_CloV(d: FunDef, env: Env): CloV = d match {
    case FunDef(n, ps, b) => CloV(ps, b, env)
    case _ => error(s"not a function definition: $d")
  }

  def VType(v : Value): Type = v match {
    case IntV(n) => IntT
    case BooleanV(b) => BooleanT
    case TupleV(vs) => TupleT
    case NilV => ListT
    case ConsV(h, t) => ListT
    case CloV(_, _, _) => FunctionT
    case ContV(c) => FunctionT
  }

  def interp(e: Expr, env: Env, k: Cont, ek: ECont): Value = e match {
    case Id(x) => k(env.getOrElse(x, error(s"free identifier: $x")))
    case IntE(n) => k(IntV(n))
    case BooleanE(b) => k(BooleanV(b))
    case Add(l, r) =>
      interp(l, env, lv =>
                     interp(r, env, rv =>
                                    k(IntVAdd(lv, rv))
                     , ek)
      , ek)
    case Mul(l, r) =>
      interp(l, env, lv =>
                     interp(r, env, rv =>
                                    k(IntVMul(lv, rv))
                     , ek)
      , ek)
    case Div(l, r) =>
      interp(l, env, lv =>
                     interp(r, env, rv =>
                                    k(IntVDiv(lv, rv))
                     , ek)
      , ek)
    case Mod(l, r) =>
      interp(l, env, lv =>
                     interp(r, env, rv =>
                                    k(IntVMod(lv, rv))
                     , ek)
      , ek)
    case Eq(l, r) =>
      interp(l, env, lv =>
                     interp(r, env, rv =>
                                    k(IntVEq(lv, rv))
                     , ek)
      , ek)
    case Lt(l, r) =>
      interp(l, env, lv =>
                     interp(r, env, rv =>
                                    k(IntVLt(lv, rv))
                     , ek)
      , ek)
    case If(c, t, f) =>
      interp(c, env, cv =>
                     cv match {
                       case BooleanV(b) => b match {
                         case true => interp(t, env, k, ek)
                         case false => interp(f, env, k, ek)
                       }
                       case v => error(s"not a boolean: $v")
                     }
      , ek)

    case TupleE(es) => es match {
      case Nil => error(s"empty tuple")
      case h :: Nil => interp(h, env, hv => k(TupleV(List(hv))), ek)
      case h :: t =>
      interp(h, env, hv =>
                     interp(TupleE(t), env, tv =>
                                    tv match {
                                      case TupleV(vs) => k(TupleV(List(hv) ++ vs))
                                    }
                     , ek)
      , ek)
    }
    case Proj(t, i) =>
      interp(t, env, tv =>
                     tv match {
                       case TupleV(vs) =>
                         if ((vs.length < i) || (i <= 0))
                           error(s"impossible index: $i")
                         k(vs(i-1))
                       case _ => error(s"not a tuple: $tv")
                     }
      , ek)
    case NilE => k(NilV)
    case ConsE(h, t) =>
      interp(h, env, hv =>
                     interp(t, env, tv =>
                                    tv match {
                                      case NilV => k(ConsV(hv, tv))
                                      case ConsV(nh, nt) => k(ConsV(hv, tv))
                                      case _ => error(s"not a list: $e")
                                    }
                     , ek)
      , ek)
    case Empty(l) =>
      interp(l, env, lv =>
                     lv match {
                       case NilV => k(BooleanV(true))
                       case ConsV(h, t) => k(BooleanV(false))
                       case _ => error(s"not a list: $lv")
                     }
      , ek)
    case Head(l) =>
      interp(l, env, lv =>
                     lv match {
                       case ConsV(h, t) => k(h)
                       case _ => error(s"empty list: $lv")
                     }
      , ek)
    case Tail(l) =>
      interp(l, env, lv =>
                     lv match {
                       case ConsV(h, t) => k(t)
                       case _ => error(s"empty list: $lv")
                     }
      , ek)
    case Val(x, e1, e2) =>
      interp(e1, env, e1v =>
                      interp(e2, env + (x -> e1v), k, ek)
      , ek)
    case Vcc(x, b) => interp(b, env + (x -> ContV(k)), k, ek)
    case Fun(ps, b) => k(CloV(ps, b, env))
    case RecFuns(ds, b) =>
      val cloVs = ds.map(FunDef_to_CloV(_, env))
      val nenv = env ++ (ds.map(_.name) zip cloVs)
      cloVs.foreach(_.env = nenv)
      interp(b, nenv, k, ek)
    case App(f, as) =>
      interp(f, env, fv =>
                     as match {
                       case Nil => error(s"no arguments")
                       case _ =>
                         interp(TupleE(as), env, atvs =>
                                                 fv match {
                                                   case CloV(ps, b, fenv) =>
                                                     if (as.length != ps.length)
                                                       error(s"wrong arity")
                                                     atvs match {
                                                       case TupleV(avs) =>
                                                         interp(b, fenv ++ (ps zip avs), k, ek)
                                                     }
                                                   case ContV(c) =>
                                                     if (as.length != 1)
                                                       error(s"wrong arity")
                                                     atvs match {
                                                       case TupleV(av) => c(av.head)
                                                     }
                                                   case _ => error(s"not a closure or continuation: $fv")
                                                 }
                         , ek)
                     }
      , ek)
    case Test(e, t) =>
      interp(e, env, v =>
                     if (VType(v) == t) k(BooleanV(true))
                     else k(BooleanV(false))
      , ek)
    case Throw(e) =>
      interp(e, env, ev =>
                     ek match {
                       case None => error(s"there's no handler")
                       case Some(h) => h(ev)
                     }
      , ek)
    case Try(e, h) =>
      interp(e, env, k, Some(x =>
        interp(h, env, hv =>
                       hv match {
                         case CloV(ps, b, fenv) =>
                           if (ps.length != 1)
                             error(s"wrong arity")
                           interp(b, fenv + (ps.head -> x), k, ek)
                         case ContV(c) =>
                           c(x)
                         case _ => error(s"wrong handler $hv")
                       }
        , ek)
      ))
      //interp(e, env, ev,) None(List(env, ek, h, k)))
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

    // test-nil
    test(run("Nil"), "Nil")
    // test-cons
    test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
    // test-isempty1
    test(run("Nil.isEmpty"), "true")
    // test-isempty2
    test(run("(1 :: Nil).isEmpty"), "false")
    // test-head
    test(run("(1 :: Nil).head"), "1")
    // test-tail
    test(run("(1 :: Nil).tail"), "Nil")
    // test-tail-head
    test(run("(1 :: 2 :: Nil).tail.head"), "2")

    // test-val1
    test(run("""
      val x = 1 + 2;
      val y = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-val2
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

    // test-vcc1
    test(run("""
      vcc x;
      1 + x(1) + 1
    """), "1")
    // test-vcc2
    test(run("""
      (x => x * x)(
        1 + vcc x; 1 + x(2) + 3
      )
    """), "9")

    // test-return1
    test(run("(x => (return 1) + x)(2)"), "1")
    // test-return2
    test(run("""
      def div(x) = (x => 10 / x)(
        if (x == 0) return 0 else x
      );
      div(0) + div(10)
    """), "1")

    // test-throw1
    testExc(run("throw 1"), "")
    // test-throw2
    testExc(run("throw throw 1"), "")

    // test-try1
    test(run("""
      try {
        throw 1
      } catch (
        x => x + x
      )
    """), "2")
    // test-try2
    test(run("""
      1 + vcc x;
        try {
          throw 1
        } catch x
    """), "2")

    // my test cases

    // test-id
    testExc(run("val x = 1; y"), "free identifier: y")
    test(run("val x = 8; x + 3"), "11")

    // test-arithmetic
    testExc(run("(x => x + x) + 3"), "not both numbers: <function>, 3")
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
    testExc(run("(1, 2, 3)._4"), "impossible index: 4")
    testExc(run("(true :: 1 :: Nil)._1"), "not a tuple: (true :: (1 :: Nil))")

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
      f(5) * g(5)
    """), "87688")

    testExc(run("def f(a) = a + a; f(1, 2)"), "wrong arity")

    // my tests
    test(run("try 1 catch x"), "1")
    test(run("(1, 2 + 3, true)._1"), "1")
    test(run("((42, 3 * 2), false)._1._2"), "6")
    test(run("try 1::throw 4::throw 2 catch v=>v"), "2")
    test(run("vcc x; {v => v + 1} + x(2)"), "2")
    test(run("vcc x; x(2) + {v => v + 1}"), "2")
    test(run("try {v => v + 1} + (throw 2) catch x => x"), "2")
    test(run("try (throw 2) + {v => v + 1}  catch x => x"), "2")
    test(run("try (throw (1 + 2)) + 4 catch x => x"), "3")

  }
}
