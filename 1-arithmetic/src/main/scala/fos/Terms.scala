package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class True(t: Term) extends Term
case class False(t: Term) extends Term
case class IsZero(t: Term) extends Term
