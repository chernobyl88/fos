package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Variable(x: String) extends Term {
  override def toString() = x
}

case class Abstraction(x: String, t: Term) extends Term {
  override def toString() = "\\" + x + "." + t 
}

case class Application(t1: Term, t2: Term) extends Term {
  override def toString() = t1 + " " + t2
}

case class Group(t: Term) extends Term {
  override def toString() = "(" + t + ")"
}