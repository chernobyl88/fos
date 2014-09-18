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

case class IsZero(t: Term) extends Term
case class Pred(t: Term) extends Term
case class Succ(t: Term) extends Term
case class If(t: Term) extends Term
case class True(t: Term) extends Term
case class False(t: Term) extends Term
case class Zero(t: Term) extends Term
