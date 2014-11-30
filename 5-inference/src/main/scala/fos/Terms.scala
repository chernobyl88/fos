package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class True() extends Term {
  override def toString() = "true"
}

case class False() extends Term {
  override def toString() = "false"
}

case class Zero() extends Term {
  override def toString() = "0"
}

case class Succ(t: Term) extends Term {
  override def toString() = "succ " + t
}

case class Pred(t: Term) extends Term {
  override def toString() = "pred " + t
}

case class IsZero(t: Term) extends Term {
  override def toString() = "iszero " + t
}

case class If(cond: Term, t1: Term, t2: Term) extends Term {
  override def toString() = "if " + cond + " then " + t1 + " else " + t2
}

case class Var(name: String) extends Term {
  override def toString() = name
}

case class Abs(v: String, tp: TypeTree, t: Term) extends Term {
  override def toString() = "(\\" + v + ":" + tp + "." + t + ")"
}

case class App(t1: Term, t2: Term) extends Term {
  override def toString() = t1.toString + (t2 match {
    case App(_, _) => " (" + t2.toString + ")" // left-associative
    case _         => " " + t2.toString
  })
}
case class Let(x: String, v: Term, t: Term) extends Term {
  override def toString() = "let " + x + " = " + v + " in " + t
}

/** Abstract Syntax Trees for types. */
abstract class TypeTree extends Term {
  def toType() : Type
}

case object BoolType extends TypeTree {
  override def toString() = "Bool"
  override def toType() = TypeBool
}

case object NatType extends TypeTree {
  override def toString() = "Nat"
  override def toType() = TypeNat
}

case class FunType(t1: TypeTree, t2: TypeTree) extends TypeTree {
  override def toString() = (t1 match {
    case FunType(_, _) => "(" + t1 + ")" // right-associative
    case _             => t1.toString
  }) + "->" + t2
  override def toType() = TypeFun(t1.toType, t2.toType)
}

case object EmptyType extends TypeTree {
  override def toType() = throw new Exception("No type for emptytype")
  override def toString() = "_"
}
