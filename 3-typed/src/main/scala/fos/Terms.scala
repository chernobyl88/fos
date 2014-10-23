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
  def eval(): Term 
  def fullEval(): Term
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
  override def setType(x: String, T: Type) = t.setType(x, T)
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
  override def setType(x: String, T: Type) = t.setType(x, T)
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
  override def setType(x: String, T: Type) = t.setType(x, T)
}
case class NumericSucc(t: Term) extends Term with Numeric with Value {
  override def toString() = "Succ(" + t + ")"
  override def getType() = checkInnerFunction(t.getType, TypeNat())
  override def setType(x: String, T: Type) = t.setType(x, T)
}

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
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T) && t3.setType(x, T)
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
  override def toString() = "\\" + x +":"+Ti+"." + t 
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
}

case class Application(t1: Term, t2: Term) extends Term {
  override def toString() = t1 + " " + t2
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = {
    if (t1.getType.sameType(t2.getType))
      t1.getType // TODO Est-ce qu'il faudrait une pair? {t1.getType, t2.getType}
    else
      ErrorType(t1.getType, t2.getType)
  }
}

case class Group(t: Term) extends Term {
  override def toString() = "(" + t + ")"
  override def setType(x: String, T: Type) = t.setType(x, T);
  override def getType() = t.getType()
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
  override def fullEval() = Pair(t1.fullEval(),t2.fullEval())
}

case class First(t: Term) extends Term {
  override def toString() = "fst" + t 

  override def setType(x: String, T: Type) = t.setType(x, T)
  override def getType() = t.getType match {
      case PairType(e, _) => FunctionType(t.getType, e.getType)
      case e => PairExpected(e)
    }
  override def eval() = t match{
    case PairType(e, _) => e.eval()
    case _ => First(t.eval())
  }
  override def fullEval() = t match {
    case PairType(e,_) => e.fullEval()
    case _ => First(t.fullEval())
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
  override def eval() = t match{
    case PairType(_, e) => e.eval()
    case _ => Second(t.eval())
  }
  override def fullEval() = t match {
    case PairType(_,e) => e.fullEval()
    case _ => Second(t.fullEval())
  }
}

trait TypeError

/** Abstract Syntax Trees for types. */
abstract class Type extends Term {
  def sameType(t1: Type) : Boolean
  def finalType() : Type
}

case class TypeBool extends Type {
  override def toString() = "Bool"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeBool() => true
    case _ => false
  }
  override def finalType() : Type = this
  override def getType() = this
}

case class NoTypeAssigned extends Type {
  override def toString() = "No type assigned"
  override def sameType(t1: Type): Boolean = t1 match {
    case NoTypeAssigned() => true
    case _ => false
  }
  override def finalType() : Type = this
  override def getType() = this
}

case class TypeNat extends Type {
  override def toString() = "Nat"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeNat() => true
    case _ => false
  }
  override def finalType() : Type = this
  override def getType() = this
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
  override def getType() = this
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
  override def getType() = this
}

case class ErrorType(t1: Type, t2: Type) extends Type with TypeError{
  override def toString() = "Error on type: Expected [" + t2 + "] and was [" + t1 + "]"
  override def sameType(t1: Type): Boolean = false
  override def finalType() : Type = this  
  override def getType() = this
}

case class AlreadyAssigned(t1: Type, t2: Type) extends Type with TypeError{
  override def toString() = "Type for var already deffined: Was [" + t1 + "] and try to assign [" + t2 + "]"
  override def sameType(t1: Type): Boolean = false
  override def finalType() : Type = this  
  override def getType() = this
}

case class PairExpected(t: Type) extends Type with TypeError{
  override def toString() = "pair type expected but " + t + " found"
  override def sameType(t1: Type): Boolean = false
  override def finalType() : Type = this  
  override def getType() = this
}
