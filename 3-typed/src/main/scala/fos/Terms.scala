package fos

import scala.util.parsing.input.Positional

trait Numeric;
trait Value;
/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term {
  override def toString() = "true"
}

case class IsZero(t: Term) extends Term {
  override def toString() = "IsZero(" + t + ")"
}

case class Pred(t: Term) extends Term {
  override def toString() = "Pred(" + t + ")"
}
case class Succ(t: Term) extends Term {
  override def toString() = "Succ(" + t + ")"
}
case class NumericSucc(t: Term) extends Term with Numeric with Value {
  override def toString() = "Succ(" + t + ")"
}

case class If(t1: Term, t2: Term, t3: Term) extends Term {
  override def toString() = "If(" + t1 + "," + t2 + "," + t3 + ")" 
}
case object False extends Term {
  override def toString() = "false"
}
case object Zero extends Term {
  override def toString() = "0"
}
case class Variable(x: String) extends Term {
  override def toString() = x
}
case class Abstraction(x: String,T:Type, t: Term) extends Term {
  override def toString() = "\\" + x +":"+T+"." + t 
}

case class Application(t1: Term, t2: Term) extends Term {
  override def toString() = t1 + " " + t2
}

case class Group(t: Term) extends Term {
  override def toString() = "(" + t + ")"
}
  //   ... To complete ... 
/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case class TypeBool extends Type {
  override def toString() = "Bool"
}

case class TypeNat extends Type {
  override def toString() = "Nat"
}

case class TypeFuncFunctionType(t1: Type, t2:Type) extends Type {
  override def toString() = t1 + "->"+ t2
}
