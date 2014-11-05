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
    if (this == temp)
      this
    else
      temp.fullEval
  }
   def subst(x: String, s: Term): Term = this
   
  def alpha(): FV = new FV(Nil)
  def equals(t1: Term) : Boolean
}

case object True extends Term with Value {
  override def toString() = "True"
  override def getType() = TypeBool()
  override def equals(t1: Term): Boolean = t1 match {
    case True => true
    case _ => false
  }
}

case class IsZero(t: Term) extends Term  {
  override def toString() = "IsZero (" + t + ")"
  override def getType() = {
    if (t.getType.sameType(TypeNat())) {
      checkInnerFunction(TypeNat(), TypeBool())
    } else {
      throw new Exception("parameter type mismatch: expected Nat, found " + t.getType + "(" + t.pos + ")")
    }
     
  }
  override def subst(x: String, s: Term) = IsZero(t.subst(x,s))
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = {
    t match {
      case Zero => True
      case Succ(e) => False
      case _ => IsZero(t.eval)
    }
  }
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case IsZero(e) => e == t
    case _ => false
  }
}

case class Pred(t: Term) extends Term {
  override def toString() = "Pred (" + t + ")"
  override def getType() = {
    if (t.getType.sameType(TypeNat())) {
      checkInnerFunction(t.getType(), TypeNat())
    } else {
      throw new Exception("parameter type mismatch: expected Nat, found " + t.getType+ "(" + t.pos + ")")
    }
  }
  override def subst(x: String, s: Term) = Pred(t.subst(x,s))
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = {
    var temp = t.eval
    if (temp == t) {
	    t match {
	      case Succ(e) => {
	        e
	      }
	      case Zero => Zero
	      case _ => Pred(temp)
	    }
    } else
      Pred(temp)
  }
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Pred(e) => e == t
    case _ => false
  }
}
case class Succ(t: Term) extends Term with Value {
  override def toString() = "Succ (" + t + ")"
  override def getType = {
    if (t.getType.sameType(TypeNat())) TypeNat()
    else {
      throw new Exception("parameter type mismatch: expected Nat, found " + t.getType+ "(" + t.pos + ")")
    }
  }
  override def subst(x: String, s: Term) = Succ(t.subst(x,s))
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
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Succ(e) => e == t
    case _ => false
  }
}

case class If(t1: Term, t2: Term, t3: Term) extends Term {
  override def toString() = "If(" + t1 + "," + t2 + "," + t3 + ")"
  override def getType = {
    if (t1.getType.sameType(TypeBool()) && t2.getType.sameType(t3.getType)) {
      FunctionType(t1.getType, t2.getType)
    } else {
      if (t1.getType.sameType(TypeBool())) {
    	  throw new Exception("parameter type mismatch: expected " + t2.getType + ", found " + t3.getType+ "(" + t3.pos + ")")
      } else {
    	  throw new Exception("parameter type mismatch: expected Bool, found " + t1.getType+ "(" + t1.pos + ")")
      }
    }
  }
  override def subst(x: String, s: Term) = If(t1.subst(x,s),t2.subst(x,s),t3.subst(x,s))
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T) && t3.setType(x, T)
  override def eval() = {
    t1 match {
      case True => t2
      case False => t3
      case _ => If(t1.eval, t2, t3)
    }
  }
  override def alpha(): FV = t1.alpha union t2.alpha union t3.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case If(e1, e2, e3) => (e1 == t1) && (e2 == t2) && (e3 == t3)
    case _ => false
  }
}

case object False extends Term with Value{
  override def toString() = "False"
  override def getType() = TypeBool()
  override def equals(t1: Term): Boolean = t1 match {
    case False => true
    case _ => false
  }
}

case object Zero extends Term with Value{
  override def toString() = "0"
  override def getType() = TypeNat()
  override def equals(t1: Term): Boolean = t1 match {
    case Zero => true
    case _ => false
  }
}

case class Variable(x: String) extends Term with Value {
  var cType: Type = null;
  override def toString() = x
  override def subst(x1: String, s: Term): Term= {
      if (x1 == x) {
        s
      } else {
        this
      }
    }
  override def setType(x1:String, T: Type) : Boolean = {
    if (x1 == x) {
      cType = T
      
    }
    true
  }
  override def getType() = {
    cType match {
      case null => NoTypeAssigned()
      case a:Type => a.getType()
    }
  }
  override def alpha(): FV = new FV(List(this))
  
  override def equals(t1: Term): Boolean = t1 match {
    case Variable(e) => e == x
    case _ => false
  }
}

case class Abstraction(x: String,T:Type, t: Term) extends Term with Value{
  var Ti = T
  t.setType(x, T)
  override def toString() = "\\" + x +":"+Ti+". (" + t + ")" 
  override def getType() = Ti
  override def setType(x1: String, T1: Type): Boolean = {
    if (x1 != x)
    	t.setType(x1, T1)
    else
    	true
  }
  override def eval() = {
    var temp = t.eval
    if (t == temp)
      this
    else
      Abstraction(x, T, temp)
  }
  override def subst(e1: String, s: Term): Term = {
      if(! (x == e1)){
	      if (s.alpha contains x) {
	        Abstraction(x+"1", T, t.subst(x, Variable(x+"1"))).subst(x, s)
	      } else {
	        Abstraction(x, T, t.subst(e1, s))
	      }
      } else {
        this
      }
    }
  override def alpha(): FV = t.alpha remove x
  
  override def equals(t1: Term): Boolean = t1 match {
    case Abstraction(x1, T, t1) => (x1 == x) && (t == t1)
    case _ => false
  }
}

case class Application(t1: Term, t2: Term) extends Term {
  override def toString() ={
    var str1 = t1 match {
      case e:Value => t1 + " "
      case _ => "(" + t1 + ") "
    }
    var str2 = t2 match {
      case e:Value => t2
      case _ => "(" + t2 + ")"
    }
    str1 + str2
  } 
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = {
    var typ1 = t1.getType() match{
    	case FunctionType(a1,a2) => t2 match{
    	  case Abstraction(_,_,_) => FunctionType(a1,a2)
    	  case _ => a1
    	}
    	case _ => t1.getType()
    }   
    var typ2 = t2 match{
    	case Abstraction(x2,ty2,a2) => {
    	  a2.getType()
    	}
    	case _ => {
    	  var temp = t2.getType()
    	  temp
    	}
    }
    if (typ1.sameType(typ2))
      t1.getType
    else
      throw new Exception("parameter type mismatch: expected " + t1.getType + ", found " + t2.getType+ "(" + t2.pos + ")")
  }
  def checkTerm(): Term = {
    var temp = t2.eval
    if (t2 == temp) {
      temp = t1.eval
      if (t1 == temp)
        this
      else
        Application(temp, t2)
    }else
       Application(t1, temp)
  }
  override def eval() = t2 match {
    case e:Value => t1 match {
      case Abstraction(x,ty,t3) => {
        var typ= t2 match{
        	case Abstraction(x2,ty2,t4) => t4.getType()
        	case _ => e.getType()
          }
        if(typ.sameType(ty)){
          t3.subst(x,e)
        } else {
        	throw new Exception("parameter type mismatch: expected " + e.getType + ", found " + ty.getType+ "(" + ty.pos + ")")
        }
      }
      case _ => checkTerm
    }
    case _ => checkTerm
  }
  override def subst(x: String, s: Term): Term = Application(t1.subst(x, s), t2.subst(x, s))
  override def alpha(): FV = t1.alpha union t2.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Application(e1, e2) => (e1 == t1) && (e2 == t2) 
    case _ => false
  }
}

case class Let(x: String,T:Type, t1: Term, t2: Term) extends Term {
  override def toString() = "let "+x+":"+T+"=("+t1+") in (" + t2 + ")"
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = {
    if (t1.getType.sameType(t2.getType))
      FunctionType(t1.getType, t2.getType)
    else
      throw new Exception("parameter type mismatch: expected " + t1.getType + ", found " + t2.getType+ "(" + t2.pos + ")")
  }
  override def eval() = {
    if(Abstraction(x,T,t2).eval() == Abstraction(x,T,t2)){
      Application(Abstraction(x,T,t2),t1).eval()
    } else {
      Application(Abstraction(x,T,t2).eval(),t1)
    }
  }
  override def subst(e1: String, s: Term): Term = {
      if(x==e1){
        Let(x, T, t1.subst(x, s), t2.subst(x, s))
      } else{
	      if (s.alpha contains x) {
	        Let(x+"1", T, t1.subst(x, Variable(x+"1")), t2.subst(x, Variable(x+"1"))).subst(x, s)
	      } else {
	        Let(x, T, t1.subst(e1, s), t2.subst(e1, s))
	      }
      }
    }
  override def alpha(): FV = t2.alpha remove x
  override def equals(t1: Term): Boolean = t1 match {
    case Let(x1, T, e1, e2) => (x == x) && (e1 == t1) && (e2 == t2)
    case _ => false
  }
}

case class Pair(t1: Term,t2: Term) extends Term {
  override def toString() = "{" + t1+", "+t2 + "}"
  override def setType(x: String, T: Type) = t1.setType(x, T) && t2.setType(x, T)
  override def getType() = PairType(t1.getType, t2.getType)
  override def eval() = {
    var temp = t1.eval
    if(temp == t1){
      Pair(t1,t2.eval())
    } else {
      Pair(temp,t2)
    }
  }
  
  override def subst(e1: String, s: Term): Term = Pair(t1.subst(e1, s), t2.subst(e1, s))
  override def alpha(): FV = t1.alpha union t2.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Pair(e1, e2) => (e1 == t1) && (e2 == t2)
    case _ => false
  }
}

case class First(t: Term) extends Term {
  override def toString() = "fst"+t 

  override def setType(x: String, T: Type) = t.setType(x, T)
  override def getType() = t.getType match {
      case PairType(e, _) => FunctionType(t.getType, e.getType)
      case e => throw new Exception("pair type expected but " + t.getType + " found"+ "(" + t.pos + ")")
    }
  override def eval() = t match{
    case Pair(e, _) => e
    case _ => First(t.eval())
  }
  override def subst(e1: String, s: Term): Term = First(t.subst(e1, s))
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case First(e) => e == t
    case _ => false
  }
}

case class Second(t: Term) extends Term {
  override def toString() = "scd" + t 
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def getType() = {
    t.getType match {
      case PairType(_, e) => FunctionType(t.getType, e.getType)
      case e => throw new Exception("pair type expected but " + t.getType + " found"+ "(" + t.pos + ")")
    }
  }
  override def eval() = t match{
    case Pair(_, e) => e.eval()
    case _ => Second(t.eval())
  }
  override def subst(e1: String, s: Term): Term = Second(t.subst(e1, s))
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Second(e) => e == t
    case _ => false
  }
}

case class Inl(t: Term, ty: Type) extends Term with Value {
  val leftType = t.getType
  val rightType = ty
  override def toString() = "Inl " + t + " as " + ty
  override def getType = new PlusType(ty, t.getType)
  override def subst(x: String, s: Term) = Inl(t.subst(x,s),ty)
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = Inl(t.eval,ty)
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Inl(e,ty2) => (e == t) && ty2.sameType(ty)
    case _ => false
  }
}

case class Inr(t: Term, ty: Type) extends Term with Value {
  val leftType = ty
  val rightType = t.getType
  override def toString() = "Inr " + t + " as " + ty
  override def getType = new PlusType(t.getType, ty)
  override def subst(x: String, s: Term) = Inr(t.subst(x,s),ty)
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = Inr(t.eval,ty)
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Inr(e,ty2) => (e == t) && ty2.sameType(ty)
    case _ => false
  }
}

case class Fix(t: Term) extends Term with Value {
  override def toString() = "Fix " + t
  override def getType = t.getType
  override def subst(x: String, s: Term) = Fix(t.subst(x,s))
  override def setType(x: String, T: Type) = t.setType(x, T)
  override def eval() = t match {
    case Abstraction(x, s, t1) =>  t1.subst(x, this)
    case _ => Fix(t.eval)
  }
  override def alpha(): FV = t.alpha
  override def equals(t1: Term): Boolean = t1 match {
    case Fix(t1) => t1 == t
    case _ => false
  }
}

case class Case(t: Term, x1: Variable,t1: Term,x2: Variable,t2: Term) extends Term{
  override def toString() = "Case " + t + " of inl " + x1 + "=>" + t1 + " | inr " + x2 + "=>" + t2
  override def getType() = {
    t match {
      case e:Inr => {
        if (t1.getType.sameType(e.leftType) && t2.getType.sameType(e.rightType))
          t.getType
        else
          throw new Exception("parameter type mismatch: expected " + e.getType + ", found " + new PlusType(t1.getType, t2.getType) + "(" + t.pos + ")")
      }
      case e:Inl => {
        if (t1.getType.sameType(e.leftType) && t2.getType.sameType(e.rightType))
          t.getType
        else
          throw new Exception("parameter type mismatch: expected " + e.getType + ", found " + new PlusType(t1.getType, t2.getType)+ "(" + t.pos + ")")
      }
      case _ => throw new Exception("parameter type mismatch: expected Inr or Inl, found " + t.getType+ "(" + t.pos + ")")
    }
  }
  override def subst(x: String, s: Term) = new Case(t.subst(x, s), x1, t1.subst(x, s), x2, t2.subst(x, s))
  override def setType(x: String, T: Type) = t.setType(x, T) && x1.setType(x, T) && t1.setType(x, T) && x2.setType(x, T) && t2.setType(x, T)
  override def eval() = {
    val evalT = t.eval
    if (evalT == t) {
      t match {
        case Inl(e, s) => t1.subst(x1.x, t)
        case Inr(e, s) => t2.subst(x2.x, t)
        case _ => throw new Exception("parameter type mismatch: expected Inr or Inl, found " + t.getType+ "(" + t.pos + ")")
      }
    } else {
     new Case(evalT, x1, t1, x2, t2) 
    }
  }
  override def alpha() = t.alpha union x1.alpha union t1.alpha union x2.alpha union t2.alpha
  override def equals(t1: Term) = t1 match {
    case Case(e1, e2, e3, e4, e5) => (e1 == t) && (e2 == x1) && (e3 == t1) && (e4 == x2) && (e5 == t2)
    case _ => false
  }
}

trait TypeError

/** Abstract Syntax Trees for types. */
abstract class Type() extends Term {
  def sameType(t1: Type) : Boolean
  def finalType() : Type = this
  override def equals(t1: Term): Boolean = false
  override def getType(): Type = this
}

case class TypeBool() extends Type {
  override def toString() = "Bool"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeBool() => true
 //   case e:FunctionType => e.sameType(this) 
    case _ => false
  }
}

case class NoTypeAssigned() extends Type {
  override def toString() = "No type assigned"
  override def sameType(t1: Type): Boolean = t1 match {
    case NoTypeAssigned() => true
 //   case e:FunctionType => e.sameType(this) 
    case _ => false
  }
}

case class TypeNat() extends Type {
  override def toString() = "Nat"
  override def sameType(t1: Type): Boolean = t1 match {
    case TypeNat() => true
  //  case e:FunctionType => e.sameType(this) 
    case _ => false
  }
}

case class FunctionType(t1: Type, t2:Type) extends Type {
  override def toString() = t1 + "->"+ t2
  override def getType(): Type = FunctionType(t1.getType(),t2.getType())
  override def sameType(t: Type): Boolean = {
    def checkFull(t: Type):Boolean = t match {
      	case FunctionType(a1, a2) => t1.sameType(a1) && t2.sameType(a2)
    	case _ => false
    }
    checkFull(t) // || t2.sameType(t) || t.sameType(t2)
  }
  override def finalType() = t2.finalType()
}

case class PairType(t1: Type, t2: Type) extends Type {
  override def toString() = t1 + " * " + t2
  override def sameType(t: Type): Boolean = {
    t match {
      case PairType(a1, a2) => t1.sameType(a1) && t2.sameType(a2)
//      case e:FunctionType => e.sameType(this) 
      case _ => false
    }
  }
  override def finalType() = PairType(t1.finalType, t2.finalType)
}

case class PlusType(t1: Type, t2: Type) extends Type {
  override def toString() = t1 + " + " + t2
  override def sameType(t: Type): Boolean = {
    t match {
      case PlusType(a1, a2) => t1.sameType(a1) && t2.sameType(a2)
      case _ => false
    }
  }
  override def finalType() = PlusType(t1.finalType, t2.finalType)
}

//Transform into Exception
/*
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
}*/

class FV(t: List[Variable]) {
  
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
    new FV(t.filter(x => !(x.toString() == s)))
  }
  
  def t() : List[Variable] = t
  
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
    new FV(recUnion(t, t1.t, List()))
  }
}