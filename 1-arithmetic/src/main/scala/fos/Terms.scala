package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

  //   ... To complete ... 
case class IsZero(t: Term) extends Term