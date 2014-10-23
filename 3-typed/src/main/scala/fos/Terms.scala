package fos

import scala.util.parsing.input.Positional

trait Numeric;
trait Value;


/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def checkInnerFunction(f: Type, end: Type) : Type = {
    f match {
      case FunctionType(a1, a2) => checkInnerFunction(a2, FunctionType(a1.finalType, end))
      case a => FunctionType(a, end)
    }
  }
  def getType() : Type
  def setType(x: String, T: Type): Boolean = true
  def eval(): Term = this
  def fullEval(): Term = {
    var temp = this.eval
    if (this.toString() == temp.toString())
      this
    else
      temp.fullEval
  }
  def checkInner(t: Term): Term = t match {
    case Group(t1) => checkInner(t1)
    case _ => t
  }
   def subst(t: Term, x: String, s: Term): Term
   = t match {
    case Variable(e1) => {
      if (e1 == x) {
        s
      } else {
        t
      }
    }
    case Abstraction(e1,ty, t1) => {
      if(x==e1){
        Abstraction(e1,ty,t1)
      } else {
        Abstraction(e1,ty, subst(t1, x, s))
      }
    }
    case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))
    case Group(t1) => Group(subst(t1, x, s))
  }
}

case object True extends Term with Value {
  override def toString() = "true"
  override def getType() = TypeBool()
}

case class IsZero(t: Term) extends Term  {
  override def toString() = "IsZero(" + t + ")"
  override def getType() = {
    if (t.getType.sameType(TypeNat())) {
      checkInnerFunction(t.getType(), TypeBool())
    } else {
      ErrorType(t.getType, TypeNat())
    }
  }
  override def subst(t1: Term, x: String, s: Term) = IsZero(t.subst(t1,x,s))
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = {
    t match {
      case Zero => True
      case e:Succ => if (e.getIsNum) False else IsZero(e.eval)
      case _ => IsZero(t.eval)
    }
  }
}

case class Pred(t: Term) extends Term {
  override def toString() = "Pred(" + t + ")"
  override def getType() = {
    if (t.getType.sameType(TypeNat())) {
      checkInnerFunction(t.getType(), TypeNat())
    } else {
      ErrorType(t.getType, TypeNat())
    }
  }
  override def subst(t1: Term, x: String, s: Term) = Pred(t.subst(t1,x,s))
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = {
    
    var temp = t.eval
    if (temp.toString == t.toString) {
	    checkInner(t) match {
	      case Succ(e) => {
	        e
	      }
	      case Zero => Zero
	      case _ => Pred(temp)
	    }
    } else
      Pred(temp)
  }
  def fullEval() = {
    t match {
      case Succ(e) => e.fullEval
      case Zero => Zero
      case _ => Pred(t.fullEval())
    }
  }
}
case class Succ(t: Term) extends Term {
  override def toString() = "Succ(" + t + ")"
  override def getType = {
    if (t.getType.sameType(TypeNat())) {
      checkInnerFunction(t.getType(), TypeNat())
    } else {
      ErrorType(t.getType, TypeNat())
    }
  }
  override def subst(t1: Term, x: String, s: Term) = Succ(t1.subst(t,x,s))
  override def setType(x: String, T: Type) = t.setType(x, T)
  def getIsNum(): Boolean = {
    def inner(e: Term): Boolean = {
	    e match {
	      case Succ(Zero) => true
	      case Succ(i:Succ) => i.getIsNum
	      case _ => false
	    }
    }
    
    inner(t)
  }
  override def eval() = Succ(t.eval)
}
/*
case class NumericSucc(t: Term) extends Term with Numeric with Value {
  override def toString() = "Succ(" + t + ")"
  override def getType() = checkInnerFunction(t.getType, TypeNat())
  override def setType(x: String, T: Type) = t.setType(x, T)
}*/

case class If(t1: Term, t2: Term, t3: Term) extends Term {
  override def toString() = "If(" + t1 + "," + t2 + "," + t3 + ")"
  override def getType = {
    if (t1.getType.sameType(TypeBool()) && t2.getType.sameType(t3.getType)) {
      FunctionType(t1.getType, t1.getType)
    } else {
      if (t1.getType.sameType(TypeBool())) {
    	  ErrorType(t2.getType, t3.getType)
      } else {
        ErrorType(t1.getType, TypeBool())
      }
    }
  }
  override def subst(t: Term, x: String, s: Term) = If(t1.subst(t,x,s),t2.subst(t,x,s),t3.subst(t,x,s))
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T) && t3.setType(x, T)
  def eval() = {
    t1 match {
      case True => t2
      case False => t3
      case _ => If(t1.eval, t2, t3)
    }
  }
}
case object False extends Term with Value{
  override def toString() = "false"
  override def getType() = TypeBool()
}
case object Zero extends Term with Value{
  override def toString() = "0"
  override def getType() = TypeNat()
}
case class Variable(x: String) extends Term with Value {
  var cType: Type = null;
  override def toString() = x
  override def subst(t: Term, x1: String, s: Term): Term= {
      if (x1 == x) {
        s
      } else {
        t
      }
    }
  override def setType(x1:String, T: Type) : Boolean = {
    if (x1 == x) {
	    this.getType match {
	      case NoTypeAssigned() => cType = T
	      case e:AlreadyAssigned => cType = AlreadyAssigned(cType, T)
	      case _ => {
	        if ((T.sameType(cType)) == false) {
	          cType = AlreadyAssigned(cType, T)
	          false
	        }
	      } 
	    }
      
    }
    true
  }
  override def getType() = {
    cType match {
      case null => NoTypeAssigned()
      case a:Type => a.getType()
    }
  }
}
case class Abstraction(x: String,T:Type, t: Term) extends Term {
  var Ti = T
  t.setType(x, T)
  override def toString() = "\\" + x +":"+Ti+". (" + t + ")" 
  override def getType() = Ti
  override def setType(x1: String, T1: Type): Boolean = {
    if (x1 != x)
    	t.setType(x1, T1)
    else
      if (Ti.getType.sameType(T1) == false) {
        Ti = AlreadyAssigned(T, T1)
        false
      } else {
        true
      }
  }
  override def eval() = {
    var temp = t.eval
    if (t.toString == temp.toString)
      this
    else
      Abstraction(x, T, temp)
  } 
}

case class Application(t1: Term, t2: Term) extends Term {
  override def toString() = "(" + t1 + ") (" + t2 + ")"
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = {
    if (t1.getType.sameType(t2.getType))
      t1.getType // TODO Est-ce qu'il faudrait une pair? {t1.getType, t2.getType}
    else
      ErrorType(t1.getType, t2.getType)
  }
  def checkTerm(): Term = {
    var temp = t2.eval
    if (t2.toString == temp.toString()) {
      temp = t1.eval
      if (t1.toString == temp.toString())
        this
      else
        Application(temp, t2)
    }else
       Application(t1, temp)
  }
  override def eval() = checkInner(t2) match {
    case e:Value => checkInner(t2) match {
      case Abstraction(x,ty,t3) =>{
        if(e.getType().sameType(ty)){
          subst(t3,x,e)
        }
        else throw new Exception // TODO
      }
      case _ => checkTerm
    }
    case _ => checkTerm
  }
}

case class Group(t: Term) extends Term {
  override def toString() = t.toString
  override def setType(x: String, T: Type) = t.setType(x, T);
  override def getType() = t.getType()
  override def eval() = checkInner(t).eval()
}

case class Let(x: String,T:Type, t1: Term, t2: Term) extends Term {
  override def toString() = "let "+x+":"+T+"="+t1+" in " + t2
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = {
    if (t1.getType.sameType(t2.getType))
      FunctionType(t1.getType, t2.getType)
    else
      ErrorType(t1.getType, t2.getType)
  }
  override def eval() = {
    if(Abstraction(x,T,t2).eval() == Abstraction(x,T,t2)){
      Application(Abstraction(x,T,t2),t1).eval()
    } else {
      Application(Abstraction(x,T,t2).eval(),t1)
    }
  }
}

case class Pair(t1: Term,t2: Term) extends Term {
  override def toString() = "{" + t1+","+t2 + "}"
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = PairType(t1.getType, t2.getType)
  override def eval() = {
    if(t1.eval() == t1){
      Pair(t1,t2.eval())
    } else {
      Pair(t1.eval(),t2)
    }
  }
}

case class First(t: Term) extends Term {
  override def toString() = "fst" + t 

  override def setType(x: String, T: Type) = t.setType(x, T)
  override def getType() = t.getType match {
      case PairType(e, _) => FunctionType(t.getType, e.getType)
      case e => PairExpected(e)
    }
  override def eval() = checkInner(t) match{
    case PairType(e, _) => e.eval()
    case _ => First(t.eval())
  }
}

case class Second(t: Term) extends Term {
  override def toString() = "scd" + t 
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def getType() = {
    t.getType match {
      case PairType(_, e) => FunctionType(t.getType, e.getType)
      case e => PairExpected(e)
    }
  }
  override def eval() = checkInner(t) match{
    case PairType(_, e) => e.eval()
    case _ => Second(t.eval())
  }
}

trait TypeError

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
}

case class NoTypeAssigned extends Type {
  override def toString() = "No type assigned"
  override def sameType(t1: Type): Boolean = t1 match {
    case NoTypeAssigned() => true
    case _ => false
  }
}

case class TypeNat extends Type {
  override def toString() = "Nat"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeNat() => true
    case _ => false
  }
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

case class PairType(t1: Type, t2: Type) extends Type {
  override def toString() = t1 + " * " + t2
  override def sameType(t: Type): Boolean = {
    t match {
      case PairType(a1, a2) => t1.sameType(a1) && t2.sameType(a2)
      case _ => false
    }
  }
  override def finalType() = PairType(t1.finalType, t2.finalType)
}

//Transform into Exception

case class ErrorType(t1: Type, t2: Type) extends Type with TypeError{
  override def toString() = "Error on type: Expected [" + t2 + "] and was [" + t1 + "]"
  override def sameType(t1: Type): Boolean = false
}

case class AlreadyAssigned(t1: Type, t2: Type) extends Type with TypeError{
  override def toString() = "Type for var already deffined: Was [" + t1 + "] and try to assign [" + t2 + "]"
  override def sameType(t1: Type): Boolean = false
}

case class PairExpected(t: Type) extends Type with TypeError{
  override def toString() = "pair type expected but " + t + " found"
  override def sameType(t1: Type): Boolean = false
}

