package cs320

package object proj03 extends Project03 {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)
  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    type MutTag = Boolean   // var or val
    case class TypeScheme(tparams: List[String], ty: Type)  // T ::= every abar.type

    // type env
    case class TypeEnv(
      vars   : List[String] = List(),
      ids    : Map[String, (TypeScheme, MutTag)] = Map(),
      tbinds : Map[String, TypeDef] = Map()
    ) {
      def addVar(x: String): TypeEnv =
        copy(vars = x :: vars)
      def addTBind(x: String, cs: TypeDef) =
        copy(tbinds = tbinds + (x -> cs))
      def addId(x: String, y: (TypeScheme, MutTag)) =
        copy(ids = ids + (x -> y))
      def contains(x: String): Boolean = {
        vars.find(_ == x) match {
          case None => false
          case _ => true
        }
      }
      def addMaps(x: TypeEnv): TypeEnv =
        copy(ids = ids ++ x.ids, tbinds = tbinds ++ x.tbinds)
      def addVars(x: List[String]): TypeEnv =
        copy(vars = vars ++ x)
    }

    // validtype
    def validType(ty: Type, tyEnv: TypeEnv): Type = ty match {
      case AppT(name, targs) =>
        targs.map(validType(_, tyEnv))
        val typedef = tyEnv.tbinds.getOrElse(name, notype(s"no such type"))
        if (targs.length != (typedef.tparams).length)
          notype(s"tparams length != targs length")
        ty
      case VarT(name) =>
        if (tyEnv.contains(name)) ty
        else notype(s"$name is a free type")
      case IntT => ty
      case BooleanT => ty
      case UnitT => ty
      case ArrowT(ptypes, rtype) =>
        ArrowT(ptypes.map(validType(_, tyEnv)), validType(rtype, tyEnv))
    }


    // helper functions for typechecker
    def mustSame(left: Type, right: Type): Type = {
      if (same(left, right)) left
      else notype(s"$left is not equal to $right")
    }
    
    def same(left:Type, right: Type): Boolean = (left, right) match {
      case (AppT(name1, targs1), AppT(name2, targs2)) =>
        (name1 == name2) && (targs1 == targs2)
      case (VarT(name1), VarT(name2)) => (name1 == name2)
      case (IntT, IntT)           => true
      case (BooleanT, BooleanT)   => true
      case (UnitT, UnitT)         => true
      case (ArrowT(ps1, r1), ArrowT(ps2, r2)) =>
        val x = (ps1 zip ps2).foldLeft(true){
          case (tf, (p1, p2)) => tf && same(p1, p2)
        }
        x && same(r1, r2)
      case _ => false
    }

    def notype(msg: Any): Nothing = error(s"no type: $msg")

    def typeCheckArithmatic(left: Expr, right: Expr, tyEnv: TypeEnv): Type = {
      mustSame(validType(typeCheckHelper(left, tyEnv), tyEnv), IntT)
      mustSame(validType(typeCheckHelper(right, tyEnv), tyEnv), IntT)
      IntT
    }
    def typeCheckLogic(left: Expr, right: Expr, tyEnv: TypeEnv): Type = {
      mustSame(validType(typeCheckHelper(left, tyEnv), tyEnv), IntT)
      mustSame(validType(typeCheckHelper(right, tyEnv), tyEnv), IntT)
      BooleanT
    }

    def subst(ty: Type, tparams: List[String], types: List[Type]): Type = {
      if (tparams.length != types.length)
        notype(s"$tparams length != $types length")
      ty match {
        case AppT(name, targs) =>
          AppT(name, targs.map(subst(_, tparams, types)))
        case VarT(name) =>
          (tparams zip types).toMap.getOrElse(name, ty)
        case IntT => ty
        case UnitT => ty
        case BooleanT => ty
        case ArrowT(ptypes, rtype) =>
          ArrowT(ptypes.map(subst(_, tparams, types)), subst(rtype, tparams, types))
      }
    }

    def typeCheckCase(c: Case, tyEnv: TypeEnv, variants: List[Variant],
                      vars: List[String], types: List[Type]): Type = {
      if (vars.length != types.length)
        notype(s"$vars length is not sames as $types length")
      variants.find(_.name == c.variant) match {
        case Some(variant) =>
          if (c.names.length != variant.params.length)
            notype(s"wrong ${c.names}")
          val ntyEnv = (variant.params zip c.names).foldLeft(tyEnv){
            case (newtyEnv, (ty, name)) => //???notlist..
              val subty = subst(ty, vars, types)
              newtyEnv.addId(name, (new TypeScheme(List(), subty), false))
          }
          validType(typeCheckHelper(c.body, ntyEnv), ntyEnv)
        case None => notype(s"no such ${c.variant}")
      }
    }
    
    def typeEnvCreate(d: RecDef): TypeEnv = d match {
      case Lazy(name, typ, expr) =>
        val emptytyEnv = new TypeEnv
        emptytyEnv.addId(name, (new TypeScheme(List(), typ), false))
      case RecFun(name, tparams, params, rtype, body) => 
        val emptytyEnv = new TypeEnv
        val paratypes = params.map(_._2)
        emptytyEnv.addId(name, (new TypeScheme(tparams, ArrowT(paratypes, rtype)), false))
      case TypeDef(name, tparams, variants) => 
        val emptytyEnv = new TypeEnv
        val tyEnv = emptytyEnv.addTBind(name, new TypeDef(name, tparams, variants))
        variants.foldLeft(tyEnv){
          case (ntyEnv, w) => w match {
            case Variant(x, List()) =>
              ntyEnv.addId(x, (new TypeScheme(tparams, AppT(name, tparams.map(VarT(_)))), false))
            case Variant(x, types) =>
              ntyEnv.addId(x, (new TypeScheme(tparams, ArrowT(types, AppT(name, tparams.map(VarT(_))))), false))
          }
        }
      //  TypeDef(
      //  name: String, tparams: List[String], variants: List[Variant]
      //  )
    }

    def validRecDef(d: RecDef, tyEnv: TypeEnv): Type = d match {
      case Lazy(name, typ, expr) => 
        validType(typ, tyEnv)
        mustSame(typ, validType(typeCheckHelper(expr, tyEnv), tyEnv))
      //Lazy(name: String, typ: Type, expr: Expr)
      case RecFun(name, tparams, params, rtype, body) =>
        tparams.foreach{ case a =>
          if (tyEnv.contains(a))
            notype(s"tyEnv already contains $a")
        }
        val ntyEnv = tyEnv.addVars(tparams)
        params.foreach{case p => validType(p._2, ntyEnv)}
        validType(rtype, ntyEnv)
        val nntyEnv = params.foldLeft(ntyEnv){
          case (newtyEnv, (name, ty)) =>
            newtyEnv.addId(name, (new TypeScheme(List(), ty), false))
        }
        mustSame(rtype, validType(typeCheckHelper(body, nntyEnv), nntyEnv))
      // RecFun(
      //  name: String,
      //  tparams: List[String],
      //  params: List[(String, Type)],
      //  rtype: Type,
      //  body: Expr
      // ) extends RecDef
      case TypeDef(name, tparams, variants) => 
        tparams.foreach{case a =>
          if (tyEnv.contains(a))
            notype(s"tyEnv already contains $a")
        }
        val ntyEnv = tyEnv.addVars(tparams)
        variants.foreach{
          case w => w.params.foreach(validType(_, ntyEnv))
        }
        UnitT // just for right return type
      //  TypeDef(
      //  name: String, tparams: List[String], variants: List[Variant]
      //  )
    }

    // typechecker helper
    def typeCheckHelper(expr: Expr, tyEnv: TypeEnv): Type = expr match {
      case Id(name, targs) =>
        targs.map(validType(_, tyEnv))
        val appname = tyEnv.ids.getOrElse(name, notype(s"no$name"))
        subst(appname._1.ty, appname._1.tparams, targs)
      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT
      case Add(left, right) => typeCheckArithmatic(left, right, tyEnv)
      case Mul(left, right) => typeCheckArithmatic(left, right, tyEnv)
      case Div(left, right) => typeCheckArithmatic(left, right, tyEnv)
      case Mod(left, right) => typeCheckArithmatic(left, right, tyEnv)
      case Eq(left, right) => typeCheckLogic(left, right, tyEnv)
      case Lt(left, right) => typeCheckLogic(left, right, tyEnv)
      case Sequence(left, right) =>
        validType(typeCheckHelper(left, tyEnv), tyEnv)
        validType(typeCheckHelper(right, tyEnv), tyEnv)
      case If(cond, texpr, fexpr) =>
        mustSame(validType(typeCheckHelper(cond, tyEnv), tyEnv), BooleanT)
        val ttype = validType(typeCheckHelper(texpr, tyEnv), tyEnv)
        mustSame(validType(typeCheckHelper(fexpr, tyEnv), tyEnv), ttype)
        ttype
      case Val(mut, name, typ, expr, body) =>
        typ match {
          case Some(ty) => validType(ty, tyEnv)
          case None => typ
        }
        val exprty = validType(typeCheckHelper(expr, tyEnv), tyEnv)
        typ match {
          case Some(ty) => mustSame(ty, exprty)
          case None => typ
        }
        val ntyEnv = tyEnv.addId(name, (new TypeScheme(List(), exprty), mut))
        validType(typeCheckHelper(body, ntyEnv), ntyEnv)
      case RecBinds(defs, body) =>
        val ntyEnv = defs.foldLeft(tyEnv){
          case (newtyEnv, d) =>
            d match {
              case TypeDef(name, tparams, variants) => 
                tyEnv.tbinds.getOrElse(name, notype(s"$name is not in tyEnv"))
              case _ => d
            }
          val dtyEnv = typeEnvCreate(d)
          newtyEnv.addMaps(dtyEnv)
        }
        defs.map(validRecDef(_, ntyEnv))
        validType(typeCheckHelper(body, ntyEnv), ntyEnv)
      case Fun(params, body) =>
        val paratypes = params.map{
          case (str, ty) => validType(ty, tyEnv)
        }
        val ntyEnv = params.foldLeft(tyEnv){
          case (newtyEnv, (str, ty)) => newtyEnv.addId(str, (new TypeScheme(List(), ty), false))
        }
        val btype = validType(typeCheckHelper(body, ntyEnv), ntyEnv)
        ArrowT(paratypes, btype)
      case Assign(name, expr) =>
        val (scheme, muttag) = tyEnv.ids.getOrElse(name, notype(s"no such $name"))
        if (scheme.tparams.length != 0)
          error(s"$name tparams length is not zero")
        if (muttag == false)
          error(s"$name is not var, but val")
        mustSame(scheme.ty, validType(typeCheckHelper(expr, tyEnv), tyEnv))
        UnitT
      case App(fun, args) => 
        val ftype = validType(typeCheckHelper(fun, tyEnv), tyEnv)
        ftype match {
          case ArrowT(ptypes, rtype) => 
            if (ptypes.length != args.length)
              notype(s"$fun arguments number is wrong")
            (ptypes zip args).foreach{
              case (ptype, arg) =>
                mustSame(ptype, validType(typeCheckHelper(arg, tyEnv), tyEnv))
            }
            rtype
          case _ => notype(s"$fun is not function type")
        }
      case Match(expr, cases) => 
        val exprtype = validType(typeCheckHelper(expr, tyEnv), tyEnv)
        exprtype match {
          case AppT(name, targs) =>
            val typedef = tyEnv.tbinds.getOrElse(name, notype(s""))
            if (targs.length != typedef.tparams.length)
              notype(s"$expr is not appropriate type application")
            if (cases.length != typedef.variants.length)
              notype(s"$cases are wrong")
            val casetypes =
              cases.map(typeCheckCase(_, tyEnv, typedef.variants, typedef.tparams, targs))
            casetypes.foreach(mustSame(_, casetypes(0)))
            casetypes(0)
          case _ => notype(s"$expr is not function application")
        }
      //Match(expr: Expr, cases: List[Case])
      //App(fun: Expr, args: List[Expr])
      //Fun(params: List[(String, Type)], body: Expr)
      //addId(x: String, y: (TypeScheme, MutTag))
    }

    def typeCheck(expr: Expr): Type = typeCheckHelper(expr, new TypeEnv)
  }

  object U {
    import Untyped._

    type Sto = Map[Addr, Value]

    // helper functions
    def IntVOp(op: (Int, Int) => Int): (Value, Value) => Value = (_, _) match {
      case (IntV(x), IntV(y)) => IntV(op(x, y))
      case (x, y) => error(s"not both numbers: $x, $y")
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

    def malloc(sto: Sto): Addr =
      maxAddress(sto) + 1
    def maxAddress(sto: Sto): Addr =
      sto.keySet.+(0).max
    
    

    //interpreter helper
    def interpHelper(expr: Expr, env: Env, sto: Sto): (Value, Sto) = expr match {
      case Id(name) =>
        val addr = env.getOrElse(name, error(s"wrong $name"))
        val value = sto.getOrElse(addr, error(s"$name has unappropriate address"))
        value match {
          case ExprV(vexpr, venv) =>
            val (vev, ves) = interpHelper(vexpr, venv, sto)
            val nsto = ves + (addr -> vev)
            (vev, nsto)
          case _ => (value, sto)
        }
      case IntE(value) => (IntV(value), sto)
      case BooleanE(value) => (BooleanV(value), sto)
      case UnitE => (UnitV, sto)
      case Add(left, right) => 
        val (lv, ls) = interpHelper(left, env, sto)
        val (rv, rs) = interpHelper(right, env, ls)
        (IntVAdd(lv, rv), rs)
      case Mul(left, right) => 
        val (lv, ls) = interpHelper(left, env, sto)
        val (rv, rs) = interpHelper(right, env, ls)
        (IntVMul(lv, rv), rs)
      case Div(left, right) => 
        val (lv, ls) = interpHelper(left, env, sto)
        val (rv, rs) = interpHelper(right, env, ls)
        (IntVDiv(lv, rv), rs)
      case Mod(left, right) => 
        val (lv, ls) = interpHelper(left, env, sto)
        val (rv, rs) = interpHelper(right, env, ls)
        (IntVMod(lv, rv), rs)
      case Eq(left, right) => 
        val (lv, ls) = interpHelper(left, env, sto)
        val (rv, rs) = interpHelper(right, env, ls)
        (IntVEq(lv, rv), rs)
      case Lt(left, right) => 
        val (lv, ls) = interpHelper(left, env, sto)
        val (rv, rs) = interpHelper(right, env, ls)
        (IntVLt(lv, rv), rs)
      case If(cond, texpr, fexpr) => 
        val (cv, cs) = interpHelper(cond, env, sto)
        cv match {
          case BooleanV(true) => interpHelper(texpr, env, cs)
          case BooleanV(false) => interpHelper(fexpr, env, cs)
          case _ => error(s"$cond is not bool") 
        }
      case Val(name, expr, body) => 
        val (ev, es) = interpHelper(expr, env, sto)
        val addr = malloc(es)
        val nenv = env + (name -> addr)
        val nsto = es + (addr -> ev)
        interpHelper(body, nenv, nsto)
      case RecBinds(defs, body) => ???
      case Fun(params, body) => (CloV(params, body, env), sto)
      case Assign(name, expr) => 
        env.getOrElse(name, error(s"$name is not in env"))
        val (ev, es) = interpHelper(expr, env, sto)
        val nsto =
    }

    //interpreter
    def interp(expr: Expr): Value = interpHelper(expr, Map(), Map())._1
  }

  def tests: Unit = {
    // test-int
    test(run("42"), "42")
    // test-boolean
    test(run("true"), "true")
    // test-unit
    test(run("()"), "()")

    // test-add
    test(run("1 + 2"), "3")
    // test-mul
    test(run("2 * 4"), "8")
    // test-div
    test(run("5 / 2"), "2")
    // test-mod
    test(run("13 % 5"), "3")
    // test-eq
    test(run("1 == 1"), "true")
    // test-lt
    test(run("1 < 1"), "false")
    // test-seq
    test(run("{1; 2}"), "2")

    // test-if
    test(run("if (true) 1 else 2"), "1")

    // test-val
    test(run("""
      val x = 1 + 2;
      val y: Int = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-lazy
    test(run("""
      lazy val f: Int => Int = (x: Int) => if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")
    // test-rec
    test(run("""
      def f(x: Int): Int = if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")

    // test-fun
    test(run("(x: Int) => x + x"), "<function>")
    // test-app
    test(run("((x: Int, y: Int) => x + y)(1, 2)"), "3")

    // test-var-assign
    test(run("""
      var x = 1;
      var y: Int = x * 4 + 8;
      { x = 3; y / (x - 1) }
    """), "6")

    // test-type-match
    test(run("""
      type Fruit {
        case Apple
        case Banana(Int)
      }
      (Apple match {
        case Apple => 1
        case Banana(x) => 0
      }) + (Banana(1) match {
        case Apple => 0
        case Banana(x) => x
      })
    """), "2")

    // test-poly1
    test(run("""
      def f['T, 'S](t: 'T, s: 'S): 'T = t;
      f[Int, Boolean](1, true)
    """), "1")
    // test-poly2
    test(run("""
      type Fruit['T] {
        case Apple
        case Banana('T)
      }
      (Apple[Boolean] match {
        case Apple => 1
        case Banana(x) => 0
      }) + (Banana[Int](1) match {
        case Apple => 0
        case Banana(x) => x
      })
    """), "2")

    // test-primitive
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

{
check(!intEquals(1, 2));
check(intEquals(3, 3));
check(intMax(3, 6) == 6);
check(intMin(3, 6) == 3);
check(!booleanEquals(true, false));
check(booleanEquals(true, true));
check(unitEquals((), ()));

score
}"""
      ),
      "7"
    )

    // test-pair
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val p1 = Pair[Int, Boolean](1, true);
val p2 = Pair[Int, Boolean](1, false);
val p3 = Pair[Int, Boolean](2, true);

val eq = pairEquals[Int, Boolean](intEquals, booleanEquals);

{
check(pairFst[Int, Boolean](p1) == 1);
check(pairSnd[Int, Boolean](p1));
check(pairFst[Int, Boolean](p2) == 1);
check(!pairSnd[Int, Boolean](p2));
check(pairFst[Int, Boolean](p3) == 2);
check(pairSnd[Int, Boolean](p3));
check(eq(p1, p1));
check(!eq(p1, p2));
check(!eq(p1, p3));

score
}"""
      ),
      "9"
    )

    // test-option
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val opt1 = Some[Int](1);
val opt2 = optionMap[Int, Int](opt1, (x: Int) => x + x);
val opt3 = optionFilter[Int](opt1, (x: Int) => x < 2);
val opt4 = optionFilter[Int](opt2, (x: Int) => x < 2);
val opt5 = optionFlatten[Int](Some[Option[Int]](opt1));
val opt6 = optionFlatten[Int](Some[Option[Int]](opt4));
val opt7 = optionFlatten[Int](None[Option[Int]]);

def aux(i: Int): Option[Int] =
  if (i == 1) Some[Int](i) else None[Int];

val opt8 = optionFlatMap[Int, Int](opt1, aux);
val opt9 = optionFlatMap[Int, Int](opt2, aux);
val opt10 = optionFlatMap[Int, Int](opt4, aux);
val opt11 = optionFilterNot[Int](opt1, (x: Int) => x < 2);
val opt12 = optionFilterNot[Int](opt2, (x: Int) => x < 2);

val eq = optionEquals[Int](intEquals);
val eql = listEquals[Int](intEquals);

{
check(eq(Some[Int](1), Some[Int](1)));
check(!eq(Some[Int](1), Some[Int](2)));
check(!eq(Some[Int](1), None[Int]));
check(eq(None[Int], None[Int]));
check(eq(opt1, Some[Int](1)));
check(eq(opt2, Some[Int](2)));
check(eq(opt3, Some[Int](1)));
check(eq(opt4, None[Int]));
check(eq(opt5, Some[Int](1)));
check(eq(opt6, None[Int]));
check(eq(opt7, None[Int]));
check(eq(opt8, Some[Int](1)));
check(eq(opt9, None[Int]));
check(eq(opt10, None[Int]));
check(eq(opt11, None[Int]));
check(eq(opt12, Some[Int](2)));
check(!optionIsEmpty[Int](opt1));
check(optionIsEmpty[Int](opt4));
check(optionNonEmpty[Int](opt1));
check(!optionNonEmpty[Int](opt4));
check(eql(optionToList[Int](opt1), List1[Int](1)));
check(eql(optionToList[Int](opt4), List0[Int]()));
check(optionGetOrElse[Int](opt1, 0) == 1);
check(optionGetOrElse[Int](opt4, 0) == 0);
optionForeach[Int](opt1, (i: Int) => check(i == 1));
optionForeach[Int](opt4, (i: Int) => check(true));

score
}"""
      ),
      "25"
    )

    // test-box
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val b = Box[Int](1);
val i1 = boxGet[Int](b);
val i2 = boxSet[Int](b, 2);
val i3 = boxGet[Int](b);
val i4 = boxSet[Int](b, 1);
val i5 = boxGet[Int](b);

{
check(i1 == 1);
check(i2 == 1);
check(i3 == 2);
check(i4 == 2);
check(i5 == 1);

score
}"""
      ),
      "5"
    )

    // test-list
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val l0 = List5[Int](1, 2, 3, 4, 5);
val l1 = List3[Int](1, 2, 3);
val l2 = List2[Int](4, 5);
val zipped0 = listZip[Int, Int](l0, l0);
val unzipped0 = listUnzip[Int, Int](zipped0);
val l3 = pairFst[List[Int], List[Int]](unzipped0);
val l4 = pairSnd[List[Int], List[Int]](unzipped0);
val zipped1 = listZip[Int, Int](l0, l1);
val unzipped1 = listUnzip[Int, Int](zipped1);
val l5 = pairFst[List[Int], List[Int]](unzipped1);
val l6 = pairSnd[List[Int], List[Int]](unzipped1);
val zipped2 = listZipWithIndex[Int](l0);
val unzipped2 = listUnzip[Int, Int](zipped2);
val l7 = pairFst[List[Int], List[Int]](unzipped2);
val l8 = pairSnd[List[Int], List[Int]](unzipped2);

val eq = listEquals[Int](intEquals);
val eqo = optionEquals[Int](intEquals);
def odd(n: Int): Boolean = n % 2 != 0;
def lt4(n: Int): Boolean = n < 4;

{
check(eq(l0, l0));
check(!eq(l0, l1));
check(!eq(l0, l2));
check(!eq(l1, l2));
check(!eq(l0, Nil[Int]));
check(eq(Nil[Int], Nil[Int]));
check(eq(listAppended[Int](listAppended[Int](l1, 4), 5), l0));
check(eq(listConcat[Int](l1, l2), l0));
check(listCount[Int](l0, odd) == 3);
check(eq(listDrop[Int](l0, 3), l2));
check(listExists[Int](l0, lt4));
check(!listExists[Int](l2, lt4));
check(eq(listFilter[Int](l0, lt4), l1));
check(eq(listFilterNot[Int](l0, lt4), l2));
check(eqo(listFind[Int](l0, lt4), Some[Int](1)));
check(eqo(listFind[Int](l2, lt4), None[Int]));
check(eq(listFlatMap[Int, Int](l1, (n: Int) => if (n == 1) l1 else if (n == 2) l2 else Nil[Int]), l0));
check(eq(listFlatten[Int](List2[List[Int]](l1, l2)), l0));
check(listFoldLeft[Int, Int](0, l0, (n: Int, m: Int) => n + m) == 15);
check(listFoldRight[Int, Int](l0, 0, (n: Int, m: Int) => n + m) == 15);
check(!listForall[Int](l0, lt4));
check(listForall[Int](l1, lt4));
listForeach[Int](l0, (n: Int) => check(odd(n)));
check(eqo(listGet[Int](l0, 4), Some[Int](5)));
check(eqo(listGet[Int](l0, 5), None[Int]));
check(!listIsEmpty[Int](l0));
check(listIsEmpty[Int](Nil[Int]));
check(listLength[Int](l0) == 5);
check(eq(listMap[Int, Int](l0, (n: Int) => n * n), List5[Int](1, 4, 9, 16, 25)));
check(listNonEmpty[Int](l0));
check(!listNonEmpty[Int](Nil[Int]));
check(eq(listPrepended[Int](listPrepended[Int](listPrepended[Int](l2, 3), 2), 1), l0));
check(eq(listReverse[Int](l0), List5[Int](5, 4, 3, 2, 1)));
check(eq(listTake[Int](l0, 3), l1));
check(eq(l0, l3));
check(eq(l0, l4));
check(eq(l1, l5));
check(eq(l1, l6));
check(eq(l0, l7));
check(eq(l0, listMap[Int, Int](l8, (n: Int) => n + 1)));

score
}"""
      ),
      "42"
    )

    // test-map
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

val m0 = Map1[Int, Int](intEquals, 0, 0);
val m1 = mapUpdated[Int, Int](m0, 1, 2);
val m2 = mapUpdated[Int, Int](m1, 2, 4);
val m3 = mapUpdated[Int, Int](m2, 3, 6);
val m4 = mapRemoved[Int, Int](m3, 2);
val m5 = mapUpdated[Int, Int](m2, 3, 8);

val eqo = optionEquals[Int](intEquals);

{
check(eqo(mapGet[Int, Int](m0, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m0, 1), None[Int]));
check(eqo(mapGet[Int, Int](m0, 2), None[Int]));
check(eqo(mapGet[Int, Int](m0, 3), None[Int]));
check(eqo(mapGet[Int, Int](m0, 4), None[Int]));

check(eqo(mapGet[Int, Int](m1, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m1, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m1, 2), None[Int]));
check(eqo(mapGet[Int, Int](m1, 3), None[Int]));
check(eqo(mapGet[Int, Int](m1, 4), None[Int]));

check(eqo(mapGet[Int, Int](m2, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m2, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m2, 2), Some[Int](4)));
check(eqo(mapGet[Int, Int](m2, 3), None[Int]));
check(eqo(mapGet[Int, Int](m2, 4), None[Int]));

check(eqo(mapGet[Int, Int](m3, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m3, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m3, 2), Some[Int](4)));
check(eqo(mapGet[Int, Int](m3, 3), Some[Int](6)));
check(eqo(mapGet[Int, Int](m3, 4), None[Int]));

check(eqo(mapGet[Int, Int](m4, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m4, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m4, 2), None[Int]));
check(eqo(mapGet[Int, Int](m4, 3), Some[Int](6)));
check(eqo(mapGet[Int, Int](m4, 4), None[Int]));

check(eqo(mapGet[Int, Int](m4, 0), Some[Int](0)));
check(eqo(mapGet[Int, Int](m4, 1), Some[Int](2)));
check(eqo(mapGet[Int, Int](m4, 2), None[Int]));
check(eqo(mapGet[Int, Int](m4, 3), Some[Int](6)));
check(eqo(mapGet[Int, Int](m4, 4), None[Int]));

score
}"""
      ),
      "30"
    )

    // test-string
    test(
      runWithStdLib(
"""var score = 0;
def check(b: Boolean): Unit =
  if (b) score = score + 1;

{
check(stringEquals("abc \n"<STRP, EOS>, List5[Int](97, 98, 99, 32, 10)));
check(stringEquals(substring("12abc \n"<STRP, EOS>, 2, 5), List3[Int](97, 98, 99)));
check("abc \n"<(n: Int, m: Int) => n + m, 0> == 336);

score
}"""
      ),
      "3"
    )

    // test-fae
    test(
      runWithStdLib(
"""
type Expr {
  case Num(Int)
  case Add(Expr, Expr)
  case Sub(Expr, Expr)
  case Id(Int)
  case Fun(Int, Expr)
  case App(Expr, Expr)
}

type Value {
  case NumV(Int)
  case CloV(Int, Expr, Map[Int, Value])
}

def interp(e: Expr, env: Map[Int, Value]): Option[Value] = e match {
  case Num(n) => Some[Value](NumV(n))
  case Add(l, r) => optionFlatMap[Value, Value](interp(l, env), (lv: Value) => lv match {
    case NumV(n) => optionFlatMap[Value, Value](interp(r, env),
      (rv: Value) => rv match {
        case NumV(m) => Some[Value](NumV(n + m))
        case CloV(x, e, fenv) => None[Value]
      }
    )
    case CloV(x, e, fenv) => None[Value]
  })
  case Sub(l, r) => optionFlatMap[Value, Value](interp(l, env), (lv: Value) => lv match {
    case NumV(n) => optionFlatMap[Value, Value](interp(r, env),
      (rv: Value) => rv match {
        case NumV(m) => Some[Value](NumV(n - m))
        case CloV(x, e, fenv) => None[Value]
      }
    )
    case CloV(x, e, fenv) => None[Value]
  })
  case Id(x) => mapGet[Int, Value](env, x)
  case Fun(x, e) => Some[Value](CloV(x, e, env))
  case App(f, a) => optionFlatMap[Value, Value](interp(f, env), (fv: Value) => fv match {
    case NumV(n) => None[Value]
    case CloV(x, e, fenv) => optionFlatMap[Value, Value](interp(a, env),
      (av: Value) => interp(e, mapUpdated[Int, Value](fenv, x, av))
    )
  })
};

lazy val digit: Parser[Expr] =
  parserMap[Int, Expr](
    () => parserCond((x: Int) => 48 <= x && x < 58),
    (x: Int) => Num(x - 48)
  );

lazy val add: Parser[Expr] =
  parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
    () => parserThen[Int, Pair[Expr, Expr]](
      () => parserConst(43),
      () => parserThen[Expr, Expr](() => e, () => e)
    ),
    (p: Pair[Int, Pair[Expr, Expr]]) =>
      pairSnd[Int, Pair[Expr, Expr]](p) match {
        case Pair(l, r) => Add(l, r)
      }
  );

lazy val sub: Parser[Expr] =
  parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
    () => parserThen[Int, Pair[Expr, Expr]](
      () => parserConst(45),
      () => parserThen[Expr, Expr](() => e, () => e)
    ),
    (p: Pair[Int, Pair[Expr, Expr]]) =>
      pairSnd[Int, Pair[Expr, Expr]](p) match {
        case Pair(l, r) => Sub(l, r)
      }
  );

lazy val id: Parser[Expr] =
  parserMap[Int, Expr](
    () => parserCond((x: Int) => 97 <= x && x <= 122),
    (x: Int) => Id(x)
  );

lazy val fun: Parser[Expr] =
  parserMap[Pair[Int, Pair[Int, Expr]], Expr](
    () => parserThen[Int, Pair[Int, Expr]](
      () => parserConst(47),
      () => parserThen[Int, Expr](
        () => parserCond((x: Int) => 97 <= x && x <= 122),
        () => e
      )
    ),
    (p: Pair[Int, Pair[Int, Expr]]) =>
      pairSnd[Int, Pair[Int, Expr]](p) match {
        case Pair(p, b) => Fun(p, b)
      }
  );

lazy val app: Parser[Expr] =
  parserMap[Pair[Int, Pair[Expr, Expr]], Expr](
    () => parserThen[Int, Pair[Expr, Expr]](
      () => parserConst(64),
      () => parserThen[Expr, Expr](() => e, () => e)
    ),
    (p: Pair[Int, Pair[Expr, Expr]]) =>
      pairSnd[Int, Pair[Expr, Expr]](p) match {
        case Pair(l, r) => App(l, r)
      }
  );

lazy val e: Parser[Expr] =
  parserOr[Expr](
    () => parserOr[Expr](
      () => parserOr[Expr](
        () => parserOr[Expr](
          () => parserOr[Expr](
            () => digit,
            () => add
          ),
          () => sub
        ),
        () => id
      ),
      () => fun
    ),
    () => app
  );

parseAll[Expr](e, "@@/x/y+xy23"<STRP, EOS>) match {
  case None => -1
  case Some(e) => interp(e, Map0[Int, Value](intEquals)) match {
    case None => -2
    case Some(v) => v match {
      case NumV(n) => if (n < 0) -3 else n
      case CloV(x, e, env) => -4
    }
  }
}
"""
      ),
      "5"
    )
  }

}
