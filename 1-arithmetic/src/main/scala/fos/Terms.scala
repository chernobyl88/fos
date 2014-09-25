package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional


/** Expr ::= 'true'
      | 'false'
      | 'if' Expr 'then' Expr 'else' Expr
      | '0'
      | 'succ' Expr
      | 'pred' Expr
      | 'iszero' Expr
   */

  //   ... To complete ... 
case class IsZero(t: Term) extends Term {
  override def toString() = "IsZero (" + t + ")"
}

case class Pred(t: Term) extends Term {
  override def toString() = "Pred (" + t + ")"
}
case class Succ(t: Term) extends Term {
  override def toString() = "Succ (" + t + ")"
}
case class If(t1: Term, t2: Term, t3: Term) extends Term {
  override def toString() = "If (" + t1 + ") then {" + t2 + "} else {" + t3 + "}" 
}
case class True() extends Term {
  override def toString() = "True"
}
case class False() extends Term {
  override def toString() = "False"
}
case class Zero() extends Term {
  override def toString() = "Zero"
}
case class StuckTerm(t: Term) extends Term {
  override def toString() = "Stuck term: " + t
}