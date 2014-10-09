package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Variable(x: String) extends Term {
  override def toString() = "Variable("+x+")"
}

case class Abstraction(x: String, t: Term) extends Term {
  override def toString() = "Abstraction(" + x + ", " + t + ")"
 // override def toString() = "\\" + x + "." + t 
}

case class Application(t1: Term, t2: Term) extends Term {
  
  override def toString() = "Application(" + t1 + ", " + t2 + ")"
  //override def toString() = t1 + " " + t2
}

case class Group(t: Term) extends Term {
  override def toString() = "Group(" + t + ")"
  //override def toString() = "(" + t + ")"
}

case class FV(t: List[Variable]) extends Term {
  
  def remove(s: String): FV = {
    FV(t.filter(x => !(x.toString() eq s)))
  }
  
  def union(t1: FV): FV = {
    def recUnion(t1: List[Variable], t2: List[Variable], ret: List[Variable]): List[Variable] = {
      t1 match {
        case e1 :: l => {
          t2.exists(p => e1.x == p.x) match {
            case true => recUnion(l, t2, e1 :: ret)
            case _ => recUnion(l, t2, ret)
          }
        }
        case _ => {
          t2 ::: ret
        }
      }
    }
    
    FV(recUnion(t, t1.t, List()))
  }
}