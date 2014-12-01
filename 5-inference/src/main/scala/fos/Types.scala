package fos

import scala.collection.immutable.{Set, ListSet}

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}
  case class TypeError(msg: String) extends Exception(msg)

case class TypeVar(name: String) extends Type
  //   ... To complete ... 
case class TypeFun(a: Type, b:Type) extends Type
  //   ... To complete ... 
case object TypeNat extends Type
  //   ... To complete ... 
case object TypeBool extends Type
  //   ... To complete ... 

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  def instantiate : Type = {
    def chaining(t: Type, tv: TypeVar, n: TypeVar): Type = t match {
      case x: TypeVar if (x == tv) => tv
      case TypeFun(a, b) => TypeFun(chaining(a, tv, n), chaining(b, tv, n))
      case _ => t
    }
    (tp /: args)(chaining(_, _, FreshName.newName()))
  }
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  //   ... To complete ... 
}


object FreshName {
  var nbr = 0
  def newName(): TypeVar = {
    nbr += 1
    new TypeVar("_" + nbr)
  }
}

abstract class Substitution extends (Type => Type) {

  var indent = 0
  
  def lookup(t: TypeVar) : Type;

  //   ... To complete ... 
  def apply(tp: Type): Type = {
    //println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
	    case x: TypeVar => lookup(x)
	    case TypeFun(a, b) => TypeFun(apply(a), apply(b)) 
	    case _ => tp
    }
    indent = indent - 1
    //println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }
  override def toString() = ""

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] = {
    env map {
      (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp)))
      }
    
  }

  def extending(t: TypeVar, T: Type): Substitution
  def checkInSet(t: TypeVar, T: Type): Boolean
}

class oneSubst(constr : List[(TypeVar, Type)]) extends Substitution {
  
  def lookup(t: TypeVar, c: List[(TypeVar, Type)]): Type = c match {
    case Nil => t
    case h :: tail => if (h._1 == t) h._2 else lookup(t, tail)
  }
  
  override def lookup(t: TypeVar) : Type = {
   lookup(t, constr)
  }
  
  override def toString() = {
    def inner(c: List[(TypeVar, Type)]) : String  = c match{
      case (a, b) :: Nil => "(" + a + " -> " + b + ")"
      case (a, b) :: tail => "(" + a + " -> " + b + ") ; " + inner(tail)
    }
    inner(constr)
  }
  
  override def checkInSet(t: TypeVar, T: Type) : Boolean = {
    def inner(constr : List[(TypeVar, Type)]) : Boolean = constr match {
      case Nil => true
      case (a:TypeVar, b:Type) :: tail => {
        if (a.name == t.name)
          if (b == T)
        	  inner(tail)
          else
             throw TypeError("Variable already instancied in Substitution for another type")
        else
          inner(tail)
      }
    }
    inner(constr)
  }
  
  override def extending(x:TypeVar, y:Type):Substitution = new oneSubst((x, y) :: constr)
}

class CoupleSubst(s1: Substitution, s2: Substitution) extends Substitution {
  var cons1 = s1
  var cons2 = s2
  override def lookup(t: TypeVar) : Type = {
    val t1 = cons1.lookup(t)
    if (t1 == t) cons2.lookup(t) else t1
  }
  
  override def apply(tp: Type) = cons2(cons1(tp))
  override def apply(tp: (Type, Type)) = cons2(cons1(tp))
  override def extending(t: TypeVar, T: Type) = cons2.extending(t: TypeVar, T: Type)
  override def checkInSet(t: TypeVar, T: Type) : Boolean = s1.checkInSet(t, T) && s2.checkInSet(t, T)
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
  override def extending(x:TypeVar, y:Type):Substitution = new oneSubst((x, y) :: Nil)
  override def checkInSet(t: TypeVar, T: Type) : Boolean = true
}
