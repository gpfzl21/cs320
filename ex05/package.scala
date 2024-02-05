package cs320

package object ex05 extends Exercise05 {

  def numOp(op: (Int, Int) => Int): (Value, Value) => Value = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numVAdd = numOp(_ + _)
  val numVSub = numOp(_ - _)

  def lookup(name: String, env: Env): Value =
    env.getOrElse(name, error(s"free identifier: $name"))

  def AddEnv(fenv: Env, params: List[String], args: List[Expr], env: Env): Env =
    params match {
      case Nil =>
        args match {
          case Nil => fenv
          case h2 :: t2 => error(s"wrong arity")
        }
      case h1 :: t1 =>
        args match {
          case Nil => error(s"wrong arity")
          case h2 :: t2 =>
            AddEnv(fenv + (h1 -> interp(h2, env)), t1, t2, env)
        }
    }

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
    case Add(left, right) => numVAdd(interp(left, env), interp(right, env))
    case Sub(left, right) => numVSub(interp(left, env), interp(right, env))
    case Id(x) => lookup(x, env)
    case App(func, args) => interp(func, env) match {
      case CloV(params, body, fenv) =>
        interp(body, AddEnv(fenv, params, args, env))
      case v =>
        error(s"not a closure: $v")
    }
    case Fun(params, body) => CloV(params, body, env)
    case Rec(rec) => RecV(rec.map { case (f, e) => (f, interp(e, env))})
    case Acc(expr, name) => interp(expr, env) match {
      case RecV(rec) =>
        rec.getOrElse(name, error(s"no such field"))
      case v =>
        error(s"not a record: $v")
    }
  }

  def tests: Unit = {
    test(run("{ (x, y) => (x + y) }(1, 2)"), "3")
    test(run("{ () => (3 + 4) }()"), "7")
    testExc(run("{ (x, y) => (x + y) }(1)"), "wrong arity")
    test(run("{ x = 1, y = 2 }.x"), "1")
    testExc(run("{ x = 1, y = 2 }.z"), "no such field")
    testExc(run("{ x = { y = 1 }.z }"), "no such field")
    test(run("42"), "42")
    test(run("{ x => x }"), "function")
    test(run("{ x = 1 }"), "record")

    /* Write your own tests */
  }
}
