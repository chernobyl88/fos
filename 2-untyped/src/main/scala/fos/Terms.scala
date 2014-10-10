package fos

trait finalTerm

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

case class FV(t: List[Variable]) extends Term {
  
  override def toString() = {
    def recur(t: List[Variable]) : String = t match {
      case e1 :: e2 :: elem => e1.toString + ", " + recur(e2 :: elem)
      case e1 :: Nil => e1.toString + "]"
      case Nil => "]"
    }
    
    "[" + recur(t)
  }
  
  def contains(x: String): Boolean = {
    t.exists(p => p.x == x)
  }
  
  def remove(s: String): FV = {
    FV(t.filter(x => !(x.toString() eq s)))
  }
  
  def union(t1: FV): FV = {
    def recUnion(t1: List[Variable], t2: List[Variable], ret: List[Variable]): List[Variable] = {
      t1 match {
        case e1 :: l => {
          t2.exists(p => e1.x == p.x) match {
            case true => recUnion(l, t2, ret)
            case false => recUnion(l, t2, e1 :: ret)
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