package cs320

trait Midterm extends Homework {
  sealed trait Expr
  case class Num(num: Int) extends Expr                                         // e ::= n
  case class Id(name: String) extends Expr                                      //     | x
  case class Val(name: String, value: Expr, body: Expr) extends Expr            //     | {val x=e;e}
  case class Add(left: Expr, right: Expr) extends Expr                          //     | (e+e)
  case class Sub(left: Expr, right: Expr) extends Expr                          //     | (e-e)
 // case class Mul(left: Expr, right: Expr) extends Expr                          //     | (e*e)
  case class Fun(params: List[String], body: Expr) extends Expr                 //     | {(x,...,x)=>e}
  case class App(func: Expr, args: List[Expr]) extends Expr                     //     | e(e,...,e)
  case class NewBox(expr: Expr) extends Expr                                    //     | Box(e)
  case class SetBox(box: Expr, expr: Expr) extends Expr                         //     | e.set(e)
  case class OpenBox(box: Expr) extends Expr                                    //     | e.get
  case class Seqn(left: Expr, right: List[Expr]) extends Expr                   //     | {e;...;e}
  case class Struct(name: String, fields: List[String], e: Expr) extends Expr   //     | x{x,...,x};e
  case class Rec(name: String, fields: List[(String, Expr)]) extends Expr       //     | x{x=e,...,x=e}
  case class Get(record: Expr, field: String) extends Expr                      //     | e.x
  case class Set(record: Expr, field: String, e: Expr) extends Expr             //     | {e.x=e}
  case class OverAdd(struct: String, oper: Expr, e: Expr) extends Expr          //     | plus x=e;e
  case class OverSub(struct: String, oper: Expr, e: Expr) extends Expr          //     | plus x=e;e

  // environment
  type Addr = Int
  type Env = Map[String, Value]
  type Sto = Map[Addr, Value]
  type Stru = Map[String, List[String]]
  type Oper = Map[(String, String), Value]

  // value type
  trait Value
  case class NumV(n: Int) extends Value
  case class CloV(params: List[String], body: Expr, env: Env) extends Value
  case class BoxV(addr: Addr) extends Value
  case class RecV(struct: String, fields: Map[String, Addr]) extends Value
  def interp(e: Expr, env: Env, stru: Stru, op:Oper, sto: Sto): (Value, Sto)
}


