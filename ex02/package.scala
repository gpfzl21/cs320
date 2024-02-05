package cs320

package object ex02 extends Exercise02 {

  // Problem 1
  def freeIds(expr: Expr): Set[String] = expr match {
    case Num(n) => Set()
    case Add(l, r) => freeIds(l) ++ freeIds(r)
    case Sub(l, r) => freeIds(l) ++ freeIds(r)
    case Val(x, i, b) => freeIds(i) ++ freeIds(b) -- Set(x)
    case Id(x) => Set(x)
  }

  // Problem 2
  def bindingIds(expr: Expr): Set[String] = expr match {
    case Num(n) => Set()
    case Add(l, r) => bindingIds(l).union(bindingIds(r))
    case Sub(l, r) => bindingIds(l).union(bindingIds(r))
    case Val(x, i, b) => Set(x).union(bindingIds(b))
    case Id(x) => Set()
  }

  // Problem 3
  def boundIds(expr: Expr): Set[String] = expr match {
    case Num(n) => Set()
    case Add(l, r) => boundIds(l) ++ boundIds(r)
    case Sub(l, r) => boundIds(l) ++ boundIds(r)
    case Val(x, i, b) => boundIds(i) ++ freeIds(b).intersect(Set(x))
    case Id(x) => Set()
  }

  // Tests
  def tests: Unit = {
    test(freeIds(Expr("{ val x = 1; (x + y) }")), Set("y"))
    test(freeIds(Expr("{ val z = 2; 1 }")), Set())
    test(bindingIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(bindingIds(Expr("{ val z = 2; 1 }")), Set("z"))
    test(boundIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(boundIds(Expr("{ val z = 2; 1 }")), Set())

    /* Write your own tests */
    test(boundIds(Expr("{ val x = 1; (x + { val x = 1; (y + 1) }) }")), Set("x"))
    test(boundIds(Expr("{ val x = 1; { val x = 1; (y + 1) } }")), Set())

    test(freeIds(Expr("(x + { val x = 1; x })")), Set("x"))
    test(bindingIds(Expr("(x + { val x = 1; x })")), Set("x"))
    test(boundIds(Expr("(x + { val x = 1; x })")), Set("x"))

    test(freeIds(Expr("(x + { val x = 1; 1 })")), Set("x"))
    test(bindingIds(Expr("(x + { val x = 1; 1 })")), Set("x"))
    test(boundIds(Expr("(x + { val x = 1; 1 })")), Set())

    test(boundIds(Expr("(x + { val x = 1; (x + x) })")), Set("x"))
  }
}
