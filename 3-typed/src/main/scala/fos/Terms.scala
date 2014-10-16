package fos

import scala.util.parsing.input.Positional

trait Numeric;
trait Value;
/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def getType() : Type
}

case object True extends Term {
  override def toString() = "true"
  override def getType() = TypeBool()
}

case class IsZero(t: Term) extends Term {
  override def toString() = "IsZero(" + t + ")"
  override def getType() = FunctionType(TypeNat(), TypeBool())
}

case class Pred(t: Term) extends Term {
  override def toString() = "Pred(" + t + ")"
  override def getType() = FunctionType(TypeNat(), TypeNat())
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

case class Let(x: String,T:Type, t1: Term, t2: Term) extends Term {
  override def toString() = "let "+x+":"+T+"="+t1+" in " + t2
}

case class Pair(t1: Term,t2: Term) extends Term {
  override def toString() = "{" + t1+","+t2 + "}"
}

case class First(t: Term) extends Term {
  override def toString() = "fst" + t 
}

case class Second(t: Term) extends Term {
  override def toString() = "snd" + t 
}
  //   ... To complete ... 
/** Abstract Syntax Trees for types. */
abstract class Type extends Term {
  def sameType(t1: Type) : Boolean
  def finalType() : Type = this
}

case class TypeBool extends Type {
  override def toString() = "Bool"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeBool() => true
    case _ => false
  }
  override def finalType() : Type = this
}

case class TypeNat extends Type {
  override def toString() = "Nat"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeNat() => true
    case _ => false
  }
  override def finalType() : Type = this
}

case class FunctionType(t1: Type, t2:Type) extends Type {
  override def toString() = t1 + "->"+ t2
  override def sameType(t: Type): Boolean = {
    def checkFull(t: Type):Boolean = t match {
      	case FunctionType(a1, a2) => t1.sameType(a1) && t2.sameType(a2)
    	case _ => false
    }
    checkFull(t) || t2.sameType(t) || t.sameType(this)
  }
  override def finalType() = t2.finalType()
}
