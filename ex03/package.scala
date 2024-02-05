package cs320

import cs320._

package object ex03 extends Exercise03 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l, r)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def minval(a: Int, b: Int): Int = {
    if (a > b)
      b
    else
      a
  }

  def maxval(a: Int, b: Int): Int = {
    if (a < b)
      b
    else
      a
  }

//  def triOp(
//    op: (Int, Int, Int) => Int,
//    ls: List[Int]
//    ms: List[Int]
//    rs: List[Int]
//  ): List[Int] = ls match {
//    case Nil => Nil
//    case l :: rest =>
//      ms match {
//        case Nil => Nil
//        case m :: rest =>
//          def f(r: Int): Int = op(l, m, r)
//          
//      }
//  }

  def lookup(name: String, env: Env): List[Int] = {
    env.getOrElse(name, error(s"free identifier: $name"))
  }

  def interp(expr: Expr, env: Env): List[Int] = expr match {
    case Num(nums) => nums
    case Add(l, r) => binOp((x: Int, y: Int) => x + y, interp(l, env), interp(r, env))
    case Sub(l, r) => binOp((x: Int, y: Int) => x - y, interp(l, env), interp(r, env))
    case Val(x, i, b) => interp(b, env + (x -> interp(i, env)))
    case Id(x) => lookup(x, env)
    case Min(l, m, r) =>
      val x: List[Int] = binOp(minval, interp(l, env), interp(m, env))
      binOp(minval, x, interp(r, env))
    case Max(l, m, r) =>
      val x: List[Int] = binOp(maxval, interp(l, env), interp(m, env))
      binOp(maxval, x, interp(r, env))
  }

  def tests: Unit = {
    test(run("(3 + 7)"), List(10))
    test(run("(10 - (3, 5))"), List(7, 5))
    test(run("{ val x = (5 + 5); (x + x) }"), List(20))
    test(run("min(3, 4, 5)"), List(3))
    test(run("max((1 + 2), 4, 5)"), List(5))
    test(run("min((1, 4), (2, 9), 3)"), List(1, 1, 2, 3))
    test(run("max((1, 6), (2, 5), (3, 4))"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
  }
}
